module Main where
  import qualified Data.Set as Set
  import WhitherPermutations
  import WhitherTSP
  import WhitherOTP (defaultOTP)
  import Data.Time.Clock (addUTCTime, UTCTime(..))
  import Data.Time.Calendar (fromGregorian)

  allStations = map (\(s,c) -> Station s c) [ 
      ("Richmond", "1:RICH")
    , ("Antioch", "1:ANTC")
    , ("Dublin/Pleasanton", "1:DUBL")
    , ("Berryessa / North San Jose", "1:BERY")
    , ("Oakland International Airport Station", "1:OAKL")
    , ("San Francisco International Airport", "1:SFIA")
    , ("Millbrae", "1:MLBR")
    ]

  requiredDestinations :: Set.Set Station
  requiredDestinations = Set.fromList allStations

  getStation :: String -> Station
  getStation stationName = head $ filter (\s -> name s == stationName) allStations

  -- 2300 UTC = 1600 PDT
  myStartTime = UTCTime (fromGregorian 2023 09 01) (15*60*60 + 0*60)
  -- BART is small so this does not matter.
  myDeadline = addUTCTime (60 * 60 * 25) myStartTime

  millbraeTest1 = twoAdjacent (getStation "San Francisco International Airport") (getStation "Millbrae")
  millbraeTest2 = setNotAtBeginningOrEnd (Set.fromList (map getStation ["San Francisco International Airport", "Millbrae"])) (length requiredDestinations)
  fremontTest = twoAdjacent (getStation "Berryessa / North San Jose") (getStation "Dublin/Pleasanton")
  allBARTTests = chainTests millbraeTest1 $ chainTests millbraeTest2 fremontTest

  main :: IO()
  main = do
    (startIndex, numToTry) <- readTwoInts
    mainBruteForce defaultOTP allBARTTests defaultPlanFlags myStartTime myDeadline requiredDestinations startIndex numToTry "/usr/share/zoneinfo/US/Pacific"






