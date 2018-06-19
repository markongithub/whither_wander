module Main where
  import qualified Data.Set as Set
  import WhitherPermutations
  import WhitherTSP
  import WhitherOTP (defaultOTP)
  import Data.Time.Clock (addUTCTime, UTCTime(..))
  import Data.Time.Calendar (fromGregorian)

  allStations = map (\(s,c) -> Station s c) [ 
    ("Richmond", "1:RICH"),
    ("Pittsburg/Bay Point", "1:PITT"),
    ("Dublin/Pleasanton", "1:DUBL"),
    ("Warm Springs/South Fremont", "1:WARM"),
    ("San Francisco International Airport", "1:SFIA"),
    ("Millbrae", "1:MLBR")]

  requiredDestinations :: Set.Set Station
  requiredDestinations = Set.fromList allStations

  getStation :: String -> Station
  getStation stationName = head $ filter (\s -> name s == stationName) allStations

  -- 2300 UTC = 1600 PDT
  myStartTime = UTCTime (fromGregorian 2017 08 31) (23*60*60)
  -- BART is small so this does not matter.
  myDeadline = addUTCTime (60 * 60 * 25) myStartTime

  millbraeTest = twoAdjacent (getStation "San Francisco International Airport") (getStation "Millbrae")
  fremontTest = twoAdjacent (getStation "Warm Springs/South Fremont") (getStation "Dublin/Pleasanton")
  allBARTTests = chainTests millbraeTest fremontTest

  main :: IO()
  main = mainBruteForce defaultOTP allBARTTests defaultPlanFlags myStartTime myDeadline requiredDestinations "/usr/share/zoneinfo/US/Pacific"
