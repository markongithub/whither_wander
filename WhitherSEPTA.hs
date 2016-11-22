module Main where
  import qualified Data.Set as Set
  import WhitherPermutations
  import WhitherTSP
  import WhitherOTP (defaultOTP, lRoute, lTo, OTPLeg, pName)
  import Data.Time.Clock (addUTCTime, UTCTime(..))
  import Data.Time.Calendar (fromGregorian)


  allStations = map (\(s,c) -> Station s c) [ 
    ("Airport",  "1:90401"),
    ("Chestnut Hill East",  "1:90720"),
    ("Chestnut Hill West",  "1:90801"),
    ("Cynwyd",  "1:90001"),
    ("Doylestown",  "1:90538"),
    ("Elwyn",  "1:90301"),
    ("Fox Chase",  "1:90815"),
    ("Newark",  "1:90201"),
    ("Norristown Elm Street",  "1:90228"),
    ("Thorndale" ,  "1:90501"),
    ("Trenton",  "1:90701"),
    ("Warminster",  "1:90417"),
    ("West Trenton",  "1:90327")]

  requiredDestinations :: Set.Set Station
  requiredDestinations = Set.fromList allStations

  septaFlags :: PlanFlagMaker
  septaFlags state
    | not inChestnutHill = []
    | gotHereViaCHW = ["bannedStops=1:90801"]
    | otherwise = ["bannedStops=1:90720"]
    where inChestnutHill = (take 13 $ name $ currentLocation state) == "Chestnut Hill"
          isCHWLeg :: OTPLeg -> Bool
          isCHWLeg leg = (lRoute leg == "Chestnut Hill West Line") && ((pName $ lTo leg) == "Chestnut Hill West")
          gotHereViaCHW = any isCHWLeg $ legsSoFar state



  getStation :: String -> Station
  getStation stationName = head $ filter (\s -> name s == stationName) allStations
  chestnutHillTest = chainTests (twoAdjacent (getStation "Chestnut Hill East") (getStation "Chestnut Hill West")) (setMustBeginBy (Set.fromList [getStation "Chestnut Hill East", getStation "Chestnut Hill West"]) 10)

  jenkintownTest = setAdjacent $ Set.fromList (map getStation ["Doylestown", "Warminster", "West Trenton"])

  -- not sure this is really optimal
  northPhillyTest = twoAdjacent (getStation "Chestnut Hill West") (getStation "Trenton")

  allSEPTATests = chainTests chestnutHillTest jenkintownTest

  myStartTime = UTCTime (fromGregorian 2016 10 14) (8*60*60) -- 8 AM UTC = 4 AM EDT
  myDeadline = addUTCTime (60 * 60 * 25) myStartTime

  main :: IO()
  main = mainBruteForce defaultOTP allSEPTATests septaFlags myStartTime myDeadline requiredDestinations "/usr/share/zoneinfo/US/Eastern"
