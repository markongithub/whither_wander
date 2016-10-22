module Main where
  import qualified Data.Set as Set
  import WhitherPermutations
  import WhitherTSP
  import WhitherOTP (defaultOTP)
  import Data.Time.Clock (addUTCTime, UTCTime(..))
  import Data.Time.Calendar (fromGregorian)

  hackettstownTest :: PermutationTest HackState Station
  hackettstownTest = PermutationTest { state = OKSoFar PreHackettstown
                                     , func = runHackOuter }

  data HackState = PreHackettstown | HeadingToHackettstown | AtHackettstown

  runHackOuter :: TestFunc HackState Station
  runHackOuter state station = runHack state (name station)
  runHack :: TestFunc HackState String
  runHack HeadingToHackettstown "Hackettstown" = OKSoFar AtHackettstown
  runHack HeadingToHackettstown _              = EarlyExit "Went toward Hackettstown then went astray."
  runHack PreHackettstown "Hackettstown" = EarlyExit "Tried to go to Hackettstown too soon"
  runHack PreHackettstown stop
    | stop == "Gladstone" || stop == "MSU" =  OKSoFar HeadingToHackettstown
    | otherwise                 = OKSoFar PreHackettstown
  runHack AtHackettstown stop
    | stop == "Gladstone" || stop == "MSU" = Finished
    | otherwise                 = EarlyExit "Got to Hackettstown then went astray."

  allStations = map (\(s,c,p) -> Station s c p) [ 
    ("Port Jervis", "1:123", False),
    ("Suffern", "1:144", False),
    ("Hackettstown", "1:54", False),
    ("Gladstone", "1:49", False),
    ("New York Penn", "1:105", False),
    ("High Bridge", "1:60", False),
    ("Bay Head", "1:13", False),
    ("Spring Valley", "1:142", False),
    ("Pearl River", "1:118", False),
    ("Trenton", "1:148", False),
    ("Princeton", "1:124", False),
    ("Radburn", "1:126", True),
    ("Clifton", "1:29", True),
    ("MSU", "1:38081", True)]

  requiredDestinations :: Set.Set Station
  requiredDestinations = Set.fromList $ filter (\x -> any (== (name x)) [
    "Bay Head",
    "Clifton",
    "Suffern",
    "Gladstone",
    "Hackettstown",
    "High Bridge",
    "MSU",
    "New York Penn",
    "Pearl River",
    "Princeton",
    "Radburn",
    "Trenton"]) allStations


  getStation :: String -> Station
  getStation stationName = head $ filter (\s -> name s == stationName) allStations
  princetonTest = twoAdjacent (getStation "Trenton") (getStation "Princeton")

  suffernTest = setAdjacent $ Set.fromList (map getStation ["Suffern", "Radburn", "Clifton"])

  allNJTests = chainTests hackettstownTest $ chainTests princetonTest suffernTest

  myFaves = tryPermutations allNJTests  requiredDestinations (3 * factorial 10 + 4 * factorial 9 + 6 * factorial 8 + 6 * factorial 7 + 2 * factorial 5 + 1 * factorial 4) (factorial 4)

  myStartTime = UTCTime (fromGregorian 2016 10 20) (8*60*60) -- 8 AM UTC = 4 AM EDT
  myDeadline = addUTCTime (60 * 60 * 25) myStartTime

  -- mainTSP otp test startTime deadline set startIndex numToTry
  main :: IO()
  main = mainTSP defaultOTP allNJTests defaultPlanFlags myStartTime myDeadline requiredDestinations
