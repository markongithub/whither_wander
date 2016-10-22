module WhitherTSP where
  import qualified Data.Set as Set
  import Control.Monad (liftM)
  import Data.List (intercalate)
  import Data.Time.Clock (addUTCTime, UTCTime(..))
  import System.Environment (getArgs)
  import WhitherOTP
  import WhitherPermutations (alwaysTrue, factorial, nextMultiple, permAtIndex, tryPermutations, PermutationOutcome(..), PermutationTest)

  data Outcome =  Outcome { verdict :: Verdict
                          , nodesLeft :: Int
                          , message :: String 
                          , finalState :: TSPState}
  instance Show Outcome where
    show outcome = let
      intro = case verdict outcome of
        Success -> "Success: "
        Failure -> "Failed with " ++ (show $ nodesLeft outcome) ++ " stops left: "
      in intro ++ message outcome

  data Verdict = Success | Failure deriving (Eq, Show)

  data TSPState = TSPState { currentTime :: UTCTime
                           , currentLocation :: Station
                           , legsSoFar :: [OTPLeg]
                           , cache :: ItineraryCache
                           }

  instance Show TSPState where
    show state = ("It is " ++ displayTime (currentTime state) ++ " and we are at " ++ (show $ currentLocation state))

  type PlanFlagMaker = TSPState -> [String]

  followItinerary :: TSPState -> Station -> OTPItinerary -> TSPState
  followItinerary curState nextDestination itin =
    curState { currentTime = (addUTCTime (fromIntegral minTransferTime) (iEndTime itin))
             , currentLocation = nextDestination 
             , legsSoFar = (legsSoFar curState) ++ (legs itin)}

  followDestinations :: OTPImpl a => a -> PlanFlagMaker -> [Station] -> UTCTime -> UTCTime -> ItineraryCache -> IO Outcome
  followDestinations otp planFlags [] utc deadline iCache = error "why would you have no destinations?"
  followDestinations otp planFlags (startPlace:remaining) utc deadline iCache =
    let firstState = TSPState{currentLocation=startPlace, currentTime=utc, legsSoFar=[], cache=iCache}
    in followDestinations0 otp planFlags remaining deadline firstState

  followDestinations0 :: OTPImpl a => a -> PlanFlagMaker -> [Station] -> UTCTime -> TSPState -> IO Outcome
  followDestinations0 _ _ [] _ state =
    return Outcome {verdict = Success, nodesLeft=0, message=show state, finalState=state}
  followDestinations0 otp planFlags (nextDest:remaining) deadline state = do
    (response, newCache) <- getFastestItineraryCaching otp (OTPPlanRequest (code $ currentLocation state) (code nextDest) (currentTime state) (planFlags state)) (cache state) 
    case response of
      Left (OTPError msg) -> return Outcome{
        verdict=Failure, nodesLeft=(1 + length remaining),
        message="No route from " ++ (show $ currentLocation state) ++ " to " ++ (show nextDest) ++ " at " ++ displayTime ((currentTime state)), finalState=state{cache=newCache}}
      Right itinerary -> do
        let nextState = followItinerary state{cache=newCache} nextDest itinerary
        if ((currentTime nextState) > deadline)
        then return Outcome {verdict=Failure, nodesLeft=(1 + length remaining),
                             message="Ran out of time. " ++ show nextState,
                             finalState = nextState}
        else followDestinations0 otp planFlags remaining deadline nextState

  data Station = Station { name :: String, code :: String, passThrough :: Bool}

  instance Ord Station where
    compare s1 s2 = compare (name s1) (name s2)

  instance Eq Station where
    s1 == s2 = (name s1) == (name s2)

  instance Show Station where
    show station = name station

  judgePermutation :: OTPImpl a => a -> PlanFlagMaker -> UTCTime -> UTCTime -> (Int, PermutationOutcome Station) -> ItineraryCache -> IO (Int, Outcome)
  judgePermutation otp planFlags startTime deadline (index, testOutput) iCache = case testOutput of
    Permutation stops -> do
      outcome <- followDestinations otp planFlags stops startTime deadline iCache
      return (index, outcome)
    PermutationFailure msg x -> return
      (index, Outcome{verdict=Failure, nodesLeft=x, message=msg, finalState=TSPState{currentLocation=(Station "this code" "is still terrible" False), currentTime=startTime, legsSoFar=[], cache=iCache}})

  judgePermutations :: OTPImpl a => a -> PermutationTest b Station -> PlanFlagMaker -> UTCTime -> UTCTime -> Set.Set Station -> Int -> Int -> IO ()
  judgePermutations otp test planFlags startTime deadline set startIndex numToTry = let
    perms = tryPermutations test set startIndex numToTry
    in judgeEach otp planFlags startTime deadline perms emptyCache
 
  dropWhileM :: Monad m => (a -> Bool) -> [m a] -> m [m a]
  dropWhileM f [] = return []
  dropWhileM f list = do
     x <- (head list)
     if (f x) then (dropWhileM f (tail list)) else return list

  judgeEach :: OTPImpl a => a -> PlanFlagMaker -> UTCTime -> UTCTime -> [(Int, PermutationOutcome Station)] -> ItineraryCache -> IO ()
  judgeEach otp planFlags startTime deadline [] _ = return ()
  judgeEach otp planFlags startTime deadline (x:xs) iCache = do
    (index, outcome) <- judgePermutation otp planFlags startTime deadline x iCache
    putStrLn (show (index, outcome))
    let newCache = cache $ finalState outcome
    let nextIndex = case (verdict outcome) of Failure -> nextMultiple index (factorial $ nodesLeft outcome)
                                              _ -> index + 1
    let newPerms = dropWhile (\(i, _) -> i < nextIndex) xs
    judgeEach otp planFlags startTime deadline newPerms newCache

  printTSPAtIndex :: OTPImpl a => Set.Set Station -> a -> PlanFlagMaker -> UTCTime -> UTCTime -> Int -> IO()
  printTSPAtIndex set otp planFlags startTime deadline index = let
    permOutcome = permAtIndex alwaysTrue set index
    in do
      (_, outcome) <- judgePermutation otp planFlags startTime deadline (index, permOutcome) emptyCache
      case permOutcome of
        Permutation stops -> putStrLn ("Order of stops: " ++ show stops)
        _ -> error "this should never happen"
      putStr $ intercalate "\n" $ showLegs $ legsSoFar $ finalState outcome

  printEach :: (Show t) => [IO t] -> IO()
  printEach [] = return ()
  printEach (x:xs) = do
    xPure <- x
    putStrLn (show xPure)
    printEach xs

  defaultPlanFlags :: PlanFlagMaker
  defaultPlanFlags _ = []

  mainTSP :: OTPImpl a => a -> PermutationTest b Station -> PlanFlagMaker -> UTCTime -> UTCTime -> Set.Set Station -> IO()
  mainTSP otp test planFlags startTime deadline set = do
    args <- getArgs
    let [startIndex, numToTry] = take 2 args
    if (numToTry == "1")
      then printTSPAtIndex set otp planFlags startTime deadline (read startIndex :: Int)
      else judgePermutations otp test planFlags startTime deadline set (read startIndex :: Int) (read numToTry :: Int)
