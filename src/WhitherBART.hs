module Main where
  import qualified Data.Set as Set
  import WhitherPermutations
  import WhitherTSP
  import WhitherOTP (defaultOTP, lRoute)
  import Data.Time.Clock (addUTCTime, UTCTime(..))
  import Data.Time.Calendar (fromGregorian)

  allStations = map (\(s,c) -> Station s c) [ 
      ("Richmond", "1:RICH")
    , ("Antioch", "1:ANTC")
    , ("Dublin/Pleasanton", "1:DUBL")
    , ("Berryessa / North San Jose", "1:BERY")
    , ("Oakland International Airport Station", "1:OAKL")
    , ("San Francisco International Airport", "1:SFIA")
--    , ("Millbrae", "1:MLBR")
    ]

  -- If I arrive at SFO on the Yellow, I have to leave on the Red, and vice
  -- versa. That way I cross all three legs of the wye.
  sfoFlags :: PlanFlagMaker
  sfoFlags state
    | not atSFO = []
    | otherwise = case lastRoute of
      "Richmond to Daly City/Millbrae" -> [("bannedRoutes", "1:8")]
      "Antioch to SFIA/Millbrae" -> [("bannedRoutes", "1:2")]
      _ -> error ("Unexpected route: " ++ lastRoute)
    where atSFO = (take (length "San Francisco International") $ name $ currentLocation state) == "San Francisco International"
          lastRoute = lRoute (last $ legsSoFar state)

  requiredDestinations :: Set.Set Station
  requiredDestinations = Set.fromList allStations

  getStation :: String -> Station
  getStation stationName = head $ filter (\s -> name s == stationName) allStations

  -- 2300 UTC = 1600 PDT
  -- myStartTime = UTCTime (fromGregorian 2023 09 05) (15*60*60 + 0*60)
  -- BART is small so this does not matter.
  -- myDeadline = addUTCTime (60 * 60 * 25) myStartTime

  -- Millbrae and SFO used to be two separate termini, but now Millbrae is
  -- treated as a stop on the Red line between SFO and San Bruno.
  -- millbraeTest1 = twoAdjacent (getStation "San Francisco International Airport") (getStation "Millbrae")
  -- millbraeTest2 = setNotAtBeginningOrEnd (Set.fromList (map getStation ["San Francisco International Airport", "Millbrae"])) (length requiredDestinations)
  fremontTest = twoAdjacent (getStation "Berryessa / North San Jose") (getStation "Dublin/Pleasanton")
  allBARTTests = fremontTest -- chainTests millbraeTest1 $ chainTests millbraeTest2 fremontTest

  main :: IO()
  main = do
    (hour, minute, startIndex, numToTry) <- readFourInts
    let startTime = UTCTime (fromGregorian 2023 09 05) (60*60*(fromIntegral hour) + 60*(fromIntegral minute))
    let deadline = addUTCTime (60 * 60 * 6 + 60 * 15) startTime
    mainBruteForce defaultOTP allBARTTests sfoFlags startTime deadline requiredDestinations startIndex numToTry "/usr/share/zoneinfo/US/Pacific"
