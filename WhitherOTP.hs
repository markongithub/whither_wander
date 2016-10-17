module WhitherOTP where

import Control.Monad (liftM, mzero)
import Data.Aeson ((.:), (.:?), eitherDecode, FromJSON, parseJSON)
import Data.Aeson.Types (Object, Value(..))
import Data.ByteString.Lazy.Internal (packChars)
import Data.List (intercalate, minimumBy)
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Data.Time.Clock (diffUTCTime, UTCTime(..))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (TimeZone(..), utcToZonedTime)
import Network.HTTP (getRequest, getResponseBody, simpleHTTP)

failEDT = TimeZone (-4 * 60) True "failEDT"
failEST = TimeZone (-5 * 60) True "failEST"

convertMillis :: Int -> UTCTime
convertMillis ms = posixSecondsToUTCTime (fromIntegral $ quot ms 1000)

queryTime :: UTCTime -> String
queryTime utc = formatTime defaultTimeLocale "&date=%Y%m%d&time=%H:%M" (utcToZonedTime failEDT utc)

displayTime :: UTCTime -> String
displayTime utc = formatTime defaultTimeLocale "%Y%m%d-%H%M %Z" (utcToZonedTime failEDT utc)

data OTPHTTPServer = OTPHTTPServer String
routerAddress = "http://localhost:8080/otp/routers/default/"

defaultOTP = OTPHTTPServer routerAddress

data OTPPlanRequest = OTPPlanRequest {
  fromStationCode :: String,
  toStationCode :: String,
  departureTime :: UTCTime,
  additionalFlags :: [String] } deriving (Eq, Ord, Show)

queryURL :: String -> OTPPlanRequest -> String
queryURL routerAddress (OTPPlanRequest fromCode toCode travelTime planFlags)
  | otherwise = url
  where url = routerAddress ++ "plan?minTransferTime=180&fromPlace=" ++ fromCode ++ "&toPlace=" ++ toCode ++ queryTime travelTime ++ extraParams
        extraParams = "&" ++ intercalate "&" planFlags

getRouteHTTP :: OTPHTTPServer -> OTPPlanRequest -> IO String
getRouteHTTP (OTPHTTPServer routerAddress) request =
  let url = queryURL routerAddress request
  in simpleHTTP (getRequest url) >>= getResponseBody

class OTPImpl a where
  getRouteJSON :: a -> OTPPlanRequest -> IO String

instance OTPImpl OTPHTTPServer where
  getRouteJSON server = getRouteHTTP server

data StaticFileReader = StaticFileReader
instance OTPImpl StaticFileReader where
  getRouteJSON _ _ = readFile "route.json"

sampleResponse :: IO OTPResponse
sampleResponse = liftM parseResponse $ readFile "route.json"

parseResponse :: String -> OTPResponse
parseResponse text =
  let bs = packChars text
      decoded = eitherDecode bs
  in case decoded of
    Left msg -> error ("Decoding failed because " ++ msg ++ " when trying to decode " ++ text)
    Right obj -> obj

data OTPError = OTPError String deriving Show

instance FromJSON OTPError where
  parseJSON (Object v) =
    OTPError <$> v .: pack "msg"
  parseJSON _ = mzero

data OTPResponse =
  OTPResponse { plan :: Maybe OTPTripPlan,
                otpError :: Maybe OTPError,
                requestParameters :: Object } deriving Show

instance FromJSON OTPResponse where
  parseJSON (Object v) =
    OTPResponse <$> v .:? pack "plan"
                <*> v .:? pack "error"
                <*> v .: pack "requestParameters"
  parseJSON _ = mzero

data OTPTripPlan = OTPTripPlan { itineraries :: [OTPItinerary] } deriving Show

instance FromJSON OTPTripPlan where
 parseJSON (Object v) =
    OTPTripPlan <$> v .: pack "itineraries"
 parseJSON _ = mzero

data OTPItinerary = OTPItinerary { iStartTime :: UTCTime,
                                   iEndTime :: UTCTime,
                                   legs :: [OTPLeg] } deriving Show

instance FromJSON OTPItinerary where
  parseJSON (Object v) =
    OTPItinerary <$> (convertMillis <$> (v .: pack "startTime"))
                 <*> (convertMillis <$> (v .: pack "endTime"))
                 <*> v .: pack "legs"
  parseJSON _ = mzero

fastestItinerary :: OTPTripPlan -> OTPItinerary
fastestItinerary plan = let
  faster x y = compare (iEndTime x) (iEndTime y)
  in minimumBy faster (itineraries plan)

type OTPItineraryOrNot = Either OTPError OTPItinerary

getFastestItinerary :: OTPImpl s => s -> OTPPlanRequest -> IO OTPItineraryOrNot
getFastestItinerary otp request = do
  response <- getRouteJSON otp request
  let parsed = parseResponse response
  case (plan parsed) of
    Just rPlan -> return $ Right (fastestItinerary rPlan)
    _          -> case (otpError parsed) of
                    Just e -> return $ Left e
                    _      -> error "No plan OR error came back from OTP."


data OTPLeg = OTPLeg { lStartTime :: UTCTime,
                       lEndTime :: UTCTime,
                       lFrom :: OTPPlace,
                       lTo :: OTPPlace,
                       lTripBlockID :: Maybe String,
                       lHeadSign :: Maybe String,
                       lRoute :: String,
                       lMode :: String}

instance FromJSON OTPLeg where
  parseJSON (Object v) =
    OTPLeg <$> (convertMillis <$> (v .: pack "startTime"))
           <*> (convertMillis <$> (v .: pack "endTime"))
           <*> v .: pack "from"
           <*> v .: pack "to"
           <*> v .:? pack "tripBlockId"
           <*> v .:? pack "headsign"
           <*> v .: pack "route"
           <*> v .: pack "mode"
  parseJSON _ = mzero

instance Show OTPLeg where
  show leg =
    let commonText = " from " ++ (pName $ lFrom leg) ++ "\n" ++ arrivalText
        railText = "Take " ++ lRoute leg ++ " train " ++ (fromMaybe "[numberless]" (lTripBlockID leg))
        walkText = "Start walking toward " ++ (pName $ lTo leg)
        timestamp = (displayTime $ lStartTime leg) ++ ": "
        arrivalText = (displayTime $ lEndTime leg) ++ ": Arrive at " ++ (pName $ lTo leg)
    in case (lMode leg) of
      "WALK" -> timestamp ++ walkText ++ commonText
      "RAIL" -> timestamp ++ railText ++ commonText
      _      -> ("What the hell is " ++ lMode leg)

trainName :: OTPLeg -> String
trainName leg = fromMaybe "[numberless]" (lTripBlockID leg)

sameTrain :: OTPLeg -> OTPLeg -> Bool
sameTrain l1 l2 = case (lTripBlockID l1, lTripBlockID l2) of
  -- If for some reason either train doesn't have a blockID we just have to
  -- assume they are different trains. Maybe this information is properly
  -- encoded by OTP but I am not looking into that now.
  (Nothing, _) -> False
  (_, Nothing) -> False
  (Just i1, Just i2) -> i1 == i2

showDeparture :: OTPLeg -> String
showDeparture leg =
  let commonText = "From " ++ (pName $ lFrom leg) ++ ", "
      railText = "take train " ++ trainName leg ++ " on " ++ routeDesc leg
      walkText = "start walking toward " ++ (pName $ lTo leg)
      timestamp = timeHeading leg
  in case (lMode leg) of
    "WALK" -> timestamp ++ commonText ++ walkText
    "RAIL" -> timestamp ++ commonText ++ railText
    _      -> ("What the shit is " ++ lMode leg)

showArrival :: OTPLeg -> String
showArrival leg = (displayTime $ lEndTime leg) ++ ": Arrive at " ++ (pName $ lTo leg) ++ "."

showLayover :: OTPLeg -> OTPLeg -> String
showLayover l1 l2 = " Kill " ++ (show minutes) ++ " minutes at " ++ (pName $ lFrom l2) ++ "."
  where minutes = quot (ceiling $ diffUTCTime (lStartTime l2) (lEndTime l1)) 60

timeHeading :: OTPLeg -> String
timeHeading leg = (displayTime $ lStartTime leg) ++ ": "

routeDesc :: OTPLeg -> String
routeDesc leg = "the " ++ lRoute leg ++ " bound for " ++ (fromMaybe "no head sign WTF" $ lHeadSign leg)

showLegs :: [OTPLeg] -> [String]
showLegs [] = []
showLegs (l:[]) = [showDeparture l, showArrival l]
showLegs (l1:(l2:xs))
-- if l1 is rail and l2 is rail and they are the same train
-- let's say the departure, no arrival, and "stay on the train" then recurse xs
  | sameTrain l1 l2 = [showDeparture l1, timeHeading l2 ++ "Stay on the train as it becomes the " ++ routeDesc l2, showArrival l2] ++ showLegs xs
-- if l1 is rail and l2 is rail and they are different trains, let's show
-- l1, the arrival+killing, and recurse l2:xs
  | (lMode l1 == "RAIL") && (lMode l2 == "RAIL") = [showDeparture l1, showArrival l1 ++ showLayover l1 l2] ++ showLegs (l2:xs)
-- if l1 leg is rail and l2 is walking, let's give the l1 departure and arrival
  | (lMode l1) == "RAIL" && (lMode l2) == "WALK" = [showDeparture l1, showArrival l1] ++ showLegs (l2:xs)
-- if l1 is walking, let's give the walking then the arrival+killing
-- then recurse on l2:xs
  | (lMode l1 == "WALK") = [showDeparture l1, showArrival l1 ++ showLayover l1 l2] ++ showLegs (l2:xs)
  | otherwise = error "I have no idea what I am doing."

data OTPPlace = OTPPlace { pName :: String } deriving Show

instance FromJSON OTPPlace where
  parseJSON (Object v) =
    OTPPlace <$> v .: pack "name"
  parseJSON _ = mzero
