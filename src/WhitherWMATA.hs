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

  stadiumTest = twoAdjacent (getStation "New Carrollton") (getStation "Largo Town Center")
  alexandriaTest = twoAdjacent (getStation "Huntington") (getStation "Franconia-Springfield")
  fallsChurchTest = twoAdjacent (getStation "Vienna") (getStation "Wiehle")
  redLineTest = twoAdjacent (getStation "Shady Grove") (getStation "Glenmont")
  greenLineTest = twoAdjacent (getStation "Branch Ave") (getStation "Greenbelt")
  southeastTest = twoAdjacent (getStation "Branch Ave") (getStation "Huntington")
  silverOrangeTest = setAdjacent $ Set.fromList (map getStation ["Vienna", "New Carrollton", "Wiehle", "Largo Town Center"])
  rosslynTest = setAdjacent $ Set.fromList (map getStation ["Vienna", "Wiehle", "Franconia-Springfield"])
  allWMATATests = chainTests stadiumTest $ chainTests alexandriaTest $ chainTests fallsChurchTest $ chainTests redLineTest $ chainTests greenLineTest $ chainTests silverOrangeTest $ chainTests rosslynTest southeastTest

  main :: IO()
  main = do
    (hour, minute, startIndex, numToTry) <- readFourInts
    let startTime = UTCTime (fromGregorian 2018 06 28) (60*60*(fromIntegral hour) + 60*(fromIntegral minute))
    let deadline = addUTCTime (60 * 60 * 7 + 60 * 30) startTime
    mainBruteForce defaultOTP allWMATATests defaultPlanFlags startTime deadline requiredDestinations startIndex numToTry "/usr/share/zoneinfo/US/Eastern"
