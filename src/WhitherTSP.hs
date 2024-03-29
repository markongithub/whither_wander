module WhitherTSP where
  import qualified Data.Set as Set
  import Data.List (intercalate)
  import Data.Maybe (fromJust, isJust, isNothing)
  import Data.Time.Clock (addUTCTime, diffUTCTime, UTCTime(..))
  import Data.Time.LocalTime.TimeZone.Olson (getTimeZoneSeriesFromOlsonFile)
  import Data.Time.LocalTime.TimeZone.Series (TimeZoneSeries(..))
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

  durationMinutes :: TSPState -> Int
  durationMinutes state = let
      durationSeconds = diffUTCTime (currentTime state) (startTime state)
      in quot (ceiling durationSeconds) 60


  data Verdict = Success | Failure deriving (Eq, Show)

  data TSPState = TSPState { currentTime :: UTCTime
                           , currentLocation :: Station
                           , legsSoFar :: [OTPLeg]
                           , cache :: ItineraryCache
                           , startTime :: UTCTime
                           }

  instance Show TSPState where
    show state = ("After " ++ (show $ durationMinutes state) ++ " minutes it is " ++ displayTime cheapUTC (currentTime state) ++ " and we are at " ++ (show $ currentLocation state))

  type PlanFlag = (String, String)
  type PlanFlagMaker = TSPState -> [PlanFlag]

  followItinerary :: TSPState -> Station -> OTPItinerary -> TSPState
  followItinerary curState nextDestination itin = let
    firstLegStartTime = lStartTime $ head $ legs itin
    newStartTime = if (null $ legsSoFar curState) then firstLegStartTime else startTime curState
    in curState { currentTime = iEndTime itin
                , currentLocation = nextDestination
                , legsSoFar = (legsSoFar curState) ++ (legs itin)
                , startTime = newStartTime
                }

  followDestinations :: OTPImpl a => a -> PlanFlagMaker -> [Station] -> UTCTime -> UTCTime -> TimeZoneSeries -> ItineraryCache -> IO Outcome
  followDestinations otp planFlags [] utc deadline tz iCache = error "why would you have no destinations?"
  followDestinations otp planFlags (startPlace:remaining) utc deadline tz iCache =
    let firstState = TSPState{currentLocation=startPlace, currentTime=utc, legsSoFar=[], cache=iCache, startTime=utc}
    in followDestinations0 otp planFlags remaining deadline tz firstState

  followDestinations0 :: OTPImpl a => a -> PlanFlagMaker -> [Station] -> UTCTime -> TimeZoneSeries -> TSPState -> IO Outcome
  followDestinations0 _ _ [] _ _ state =
    return Outcome {verdict = Success, nodesLeft=0, message=show state, finalState=state}
  followDestinations0 otp planFlags (nextDest:remaining) deadline tz state = do
    (response, newCache) <- getItineraryAlightingOrNot otp (planFlags state) tz nextDest state
    case response of
      Left (OTPError msg) -> return Outcome{
        verdict=Failure, nodesLeft=(1 + length remaining),
        message="No route from " ++ (show $ currentLocation state) ++ " to " ++ (show nextDest) ++ " at " ++ displayTime tz (currentTime state), finalState=state{cache=newCache}}
      Right itinerary -> do
        let nextState = followItinerary state{cache=newCache} nextDest itinerary
        if ((currentTime nextState) > deadline)
        then return Outcome {verdict=Failure, nodesLeft=(1 + length remaining),
                             message="Ran out of time. " ++ show nextState,
                             finalState = nextState}
        else followDestinations0 otp planFlags remaining deadline tz nextState

  getItineraryAlightingOrNot :: OTPImpl a => a -> [PlanFlag] -> TimeZoneSeries -> Station -> TSPState -> IO (OTPItineraryOrNot, ItineraryCache)
  -- this sucks
  getItineraryAlightingOrNot otp planFlags tz nextDest state
    | null (legsSoFar state) = runRequest firstRequest
    | lMode (last (legsSoFar state)) == "WALK" = runRequest firstRequest
    | isNothing (lTripBlockID lastLeg) = runRequest secondRequest
    | otherwise = chainedRequests
    where lastLeg = last (legsSoFar state)
          lastBlockID = fromJust (lTripBlockID lastLeg)
          firstLeg itin = case (legs itin) of
            [] -> error "like that guy in Kids the itinery has no legs"
            (x:xs) -> x
          stayingOnTrain itin = lMode (firstLeg itin) /= "WALK" && isJust (lTripBlockID $ firstLeg itin) && lastBlockID == fromJust (lTripBlockID $ firstLeg itin)
          firstRequest = OTPPlanRequest (code $ currentLocation state) (code nextDest) (currentTime state) tz planFlags
          runRequest r = getFastestItineraryCaching otp r (cache state)
          secondRequest = firstRequest {departureTime = addUTCTime (fromIntegral minTransferTime) (currentTime state)}
          chainedRequests = do
            (response, newCache) <- runRequest firstRequest
            case response of
              Left (OTPError msg) -> return (response, newCache)
              Right itinerary -> if stayingOnTrain itinerary then return (response, newCache) else getFastestItineraryCaching otp secondRequest newCache

  data Station = Station { name :: String, code :: String}

  instance Ord Station where
    compare s1 s2 = compare (name s1) (name s2)

  instance Eq Station where
    s1 == s2 = (name s1) == (name s2)

  instance Show Station where
    show station = name station

  judgePermutation :: OTPImpl a => a -> PlanFlagMaker -> UTCTime -> UTCTime -> TimeZoneSeries -> (Int, PermutationOutcome Station) -> ItineraryCache -> IO (Int, Outcome)
  judgePermutation otp planFlags startTime deadline tz (index, testOutput) iCache = case testOutput of
    Permutation stops -> do
      outcome <- followDestinations otp planFlags stops startTime deadline tz iCache
      return (index, outcome)
    PermutationFailure msg x -> return
      (index, Outcome{verdict=Failure, nodesLeft=x, message=msg, finalState=TSPState{currentLocation=(Station "this code" "is terrible"), currentTime=startTime, startTime=startTime, legsSoFar=[], cache=iCache}})

  judgePermutations :: OTPImpl a => a -> PermutationTest b Station -> PlanFlagMaker -> UTCTime -> UTCTime -> TimeZoneSeries -> Set.Set Station -> Int -> Int -> IO ()
  judgePermutations otp test planFlags startTime deadline tz set startIndex numToTry = let
    perms = tryPermutations test set startIndex numToTry
    in judgeEach otp planFlags startTime deadline tz perms emptyCache
 
  dropWhileM :: Monad m => (a -> Bool) -> [m a] -> m [m a]
  dropWhileM f [] = return []
  dropWhileM f list = do
     x <- (head list)
     if (f x) then (dropWhileM f (tail list)) else return list

  judgeEach :: OTPImpl a => a -> PlanFlagMaker -> UTCTime -> UTCTime -> TimeZoneSeries -> [(Int, PermutationOutcome Station)] -> ItineraryCache -> IO ()
  judgeEach otp planFlags startTime deadline _ [] _ = return ()
  judgeEach otp planFlags startTime deadline tz (x:xs) iCache = do
    (index, outcome) <- judgePermutation otp planFlags startTime deadline tz x iCache
    putStrLn (show (index, outcome))
    let newCache = cache $ finalState outcome
    let nextIndex = case (verdict outcome) of Failure -> nextMultiple index (factorial $ nodesLeft outcome)
                                              _ -> index + 1
    let newPerms = dropWhile (\(i, _) -> i < nextIndex) xs
    judgeEach otp planFlags startTime deadline tz newPerms newCache

  printTSPAtIndex :: OTPImpl a => Set.Set Station -> a -> PlanFlagMaker -> UTCTime -> UTCTime -> TimeZoneSeries -> Int -> IO()
  printTSPAtIndex set otp planFlags startTime deadline tz index = let
    permOutcome = permAtIndex alwaysTrue set index
    in do
      (_, outcome) <- judgePermutation otp planFlags startTime deadline tz (index, permOutcome) emptyCache
      case permOutcome of
        Permutation stops -> putStrLn ("Order of stops: " ++ show stops)
        _ -> error "this should never happen"
      putStr $ intercalate "\n" $ showLegs tz $ legsSoFar $ finalState outcome

  printEach :: (Show t) => [IO t] -> IO()
  printEach [] = return ()
  printEach (x:xs) = do
    xPure <- x
    putStrLn (show xPure)
    printEach xs

  defaultPlanFlags :: PlanFlagMaker
  defaultPlanFlags _ = []

  unsafeParseInt :: String -> Int
  unsafeParseInt s = (read s :: Int)

  readTwoInts :: IO (Int, Int)
  readTwoInts = do
    args <- getArgs
    let [startIndex, numToTry] = map unsafeParseInt $ take 2 args
    return (startIndex, numToTry)

  readFourInts :: IO (Int, Int, Int, Int)
  readFourInts = do
    args <- getArgs
    let fourStrings = take 4 args
    let [hour, minute, startIndex, numToTry] = map unsafeParseInt $ take 4 args
    return (hour, minute, startIndex, numToTry)

  mainBruteForce :: OTPImpl a => a -> PermutationTest b Station -> PlanFlagMaker -> UTCTime -> UTCTime -> Set.Set Station -> Int -> Int -> String -> IO()
  mainBruteForce otp test planFlags startTime deadline set startIndex numToTry tzFile = do
    tz <- getTimeZoneSeriesFromOlsonFile tzFile
    if (numToTry == 1)
      then printTSPAtIndex set otp planFlags startTime deadline tz startIndex
      else judgePermutations otp test planFlags startTime deadline tz set startIndex numToTry

  data Instruction = OTPHop Station [PlanFlag]
                     | ForcedTransfer Station Int

  followInstructions :: OTPImpl a => a -> [Instruction] -> UTCTime -> UTCTime -> String -> IO ()
  followInstructions otp [] utc deadline tzFile = error "why would you have no destinations?"
  followInstructions otp (firstInstruction:remaining) utc deadline tzFile = do
    let startPlace = (case firstInstruction of
                      OTPHop station _ -> station
                      ForcedTransfer _ _ -> error "first instruction can't be a transfer")
    let firstState = TSPState{currentLocation=startPlace, currentTime=utc, legsSoFar=[], cache=emptyCache, startTime=utc}
    tz <- getTimeZoneSeriesFromOlsonFile tzFile
    outcome <- followInstructions0 otp remaining deadline tz firstState
    putStr $ intercalate "\n" $ showLegs tz $ legsSoFar $ finalState outcome
    putStrLn $ ("\n" ++ message outcome)

  followInstructions0 :: OTPImpl a => a -> [Instruction] -> UTCTime -> TimeZoneSeries -> TSPState -> IO Outcome
  followInstructions0 _ [] _ _ state =
    return Outcome {verdict = Success, nodesLeft=0, message=show state, finalState=state}
  followInstructions0 otp (nextInstruction:remaining) deadline tz state =
    case nextInstruction of
      OTPHop nextDest planFlags -> do
        (response, newCache) <- getItineraryAlightingOrNot otp planFlags tz nextDest state
        case response of
          Left (OTPError msg) -> return Outcome{
            verdict=Failure, nodesLeft=(1 + length remaining),
            message="No route from " ++ (show $ currentLocation state) ++ " to " ++ (show nextDest) ++ " at " ++ displayTime tz (currentTime state), finalState=state{cache=newCache}}
          Right itinerary -> do
            let nextState = followItinerary state{cache=newCache} nextDest itinerary
            if ((currentTime nextState) > deadline)
            then return Outcome {verdict=Failure, nodesLeft=(1 + length remaining),
                                message="Ran out of time. " ++ show nextState,
                                finalState = nextState}
            else followInstructions0 otp remaining deadline tz nextState
      ForcedTransfer nextDest duration -> do
        let nextState = state{currentLocation = nextDest, currentTime = addUTCTime (fromIntegral duration) (currentTime state)}
        if ((currentTime nextState) > deadline)
          then return Outcome {verdict=Failure, nodesLeft=(1 + length remaining),
                              message="Ran out of time. " ++ show nextState,
                              finalState = nextState}
          else followInstructions0 otp remaining deadline tz nextState
