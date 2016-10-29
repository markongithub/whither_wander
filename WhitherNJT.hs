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

  allStations = map (\(s,c) -> Station s c) [ 
    ("Port Jervis", "1:123"),
    ("Suffern", "1:144"),
    ("Hackettstown", "1:54"),
    ("Gladstone", "1:49"),
    ("New York Penn", "1:105"),
    ("High Bridge", "1:60"),
    ("Bay Head", "1:13"),
    ("Spring Valley", "1:142"),
    ("Pearl River", "1:118"),
    ("Trenton", "1:148"),
    ("Princeton", "1:124"),
    ("Radburn", "1:126"),
    ("Clifton", "1:29"),
    ("MSU", "1:38081")]

  requiredDestinations :: Set.Set Station
  requiredDestinations = Set.fromList $ filter (\x -> any (== (name x)) [
    "Suffern",
    "Hackettstown",
    "Gladstone",
    "New York Penn",
    "High Bridge",
    "Bay Head",
    "Pearl River",
    "Trenton",
    "Princeton",
    "Radburn",
    "Clifton",
    "MSU"]) allStations


  getStation :: String -> Station
  getStation stationName = head $ filter (\s -> name s == stationName) allStations
  princetonTest = twoAdjacent (getStation "Trenton") (getStation "Princeton")

  suffernTest = setAdjacent $ Set.fromList (map getStation ["Suffern", "Radburn", "Clifton"])

  allNJTests = chainTests hackettstownTest $ chainTests princetonTest suffernTest

  myFaves = tryPermutations allNJTests  requiredDestinations (3 * factorial 10 + 4 * factorial 9 + 6 * factorial 8 + 6 * factorial 7 + 2 * factorial 5 + 1 * factorial 4) (factorial 4)

  myStartTime = UTCTime (fromGregorian 2016 9 27) (8*60*60) -- 8 AM UTC = 4 AM EDT
  myDeadline = addUTCTime (60 * 60 * 25) myStartTime

  -- mainTSP otp test startTime deadline set startIndex numToTry
  main :: IO()
  main = mainTSP defaultOTP allNJTests myStartTime myDeadline requiredDestinations
