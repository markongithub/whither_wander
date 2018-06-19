module Main where
  import qualified Data.Set as Set
  import WhitherPermutations
  import WhitherTSP
  import WhitherOTP (defaultOTP)
  import Data.Time.Clock (addUTCTime, UTCTime(..))
  import Data.Time.Calendar (fromGregorian)

  allStations = map (\(s,c) -> Station s c) [ 
    ("Shady Grove", "1:9360"),
    ("Glenmont", "1:16051"),
    ("Greenbelt", "1:11731"),
    ("New Carrollton", "1:2658"),
    ("Largo Town Center", "1:4697"),
    ("Branch Ave", "1:21110"),
    ("Huntington", "1:10776"),
    ("Franconia-Springfield", "1:14816"),
    ("Vienna", "1:11610"),
    ("Wiehle", "1:39928")]

  requiredDestinations :: Set.Set Station
  requiredDestinations = Set.fromList allStations

  getStation :: String -> Station
  getStation stationName = let
    matches = filter (\s -> name s == stationName) allStations
    in if (null matches) then (error ("the fuck is " ++ stationName)) else (head matches)
  -- 2350 UTC = 0450 EST -- this is so wrong how did that ever make sense
  myStartTime = UTCTime (fromGregorian 2018 01 03) (15*60*60 + 35*60)
  myDeadline = addUTCTime (60 * 60 * 25) myStartTime

  stadiumTest = twoAdjacent (getStation "New Carrollton") (getStation "Largo Town Center")
  alexandriaTest = twoAdjacent (getStation "Huntington") (getStation "Franconia-Springfield")
  fallsChurchTest = twoAdjacent (getStation "Vienna") (getStation "Wiehle")
  allWMATATests = chainTests stadiumTest $ chainTests alexandriaTest fallsChurchTest

  main :: IO()
  main = do
    (hour, minute, startIndex, numToTry) <- readFourInts
    let startTime = UTCTime (fromGregorian 2018 01 03) (60*60*(fromIntegral hour) + 60*(fromIntegral minute))
    let deadline = addUTCTime (60 * 60 * 25) startTime
    mainBruteForce defaultOTP allWMATATests defaultPlanFlags startTime deadline requiredDestinations startIndex numToTry "/usr/share/zoneinfo/US/Eastern"
