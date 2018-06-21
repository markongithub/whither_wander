module WhitherOTP where

import Control.Monad (liftM, mzero)
import Data.Aeson ((.:), (.:?), eitherDecode, FromJSON, parseJSON)
import Data.Aeson.Types (Object, Value(..))
import Data.ByteString.Lazy.Internal (packChars)
import Data.Char (toLower)
import Data.List (intercalate, minimumBy)
import Data.Map as Map (Map, empty, insert, lookup)
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Data.Time.Clock (diffUTCTime, UTCTime(..))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (TimeZone(..))
import Data.Time.LocalTime.TimeZone.Series (TimeZoneSeries(..), utcToLocalTime')
import Network.HTTP (getRequest, getResponseBody, simpleHTTP)

minTransferTime = 60

convertMillis :: Int -> UTCTime
convertMillis ms = posixSecondsToUTCTime (fromIntegral $ quot ms 1000)

queryTime :: TimeZoneSeries -> UTCTime -> String
queryTime tz utc = formatTime defaultTimeLocale "&date=%Y%m%d&time=%H:%M" (utcToLocalTime' tz utc)

displayTime :: TimeZoneSeries -> UTCTime -> String
displayTime tz utc = formatTime defaultTimeLocale "%Y%m%d-%H%M %Z" (utcToLocalTime' tz utc)

data OTPHTTPServer = OTPHTTPServer String
routerAddress = "http://localhost:8080/otp/routers/default/"

defaultOTP = OTPHTTPServer routerAddress

data OTPPlanRequest = OTPPlanRequest {
  fromStationCode :: String,
  toStationCode :: String,
  departureTime :: UTCTime,
  tz :: TimeZoneSeries,
  additionalFlags :: [String] } deriving (Eq, Ord, Show)

queryURL :: String -> OTPPlanRequest -> String
queryURL routerAddress (OTPPlanRequest fromCode toCode travelTime tz planFlags)
  | otherwise = url
  where url = routerAddress ++ "plan?minTransferTime=" ++ show minTransferTime ++ "&fromPlace=" ++ fromCode ++ "&toPlace=" ++ toCode ++ queryTime tz travelTime ++ extraParams
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

parseFromURL :: String -> IO OTPResponse
parseFromURL url = liftM parseResponse $ simpleHTTP (getRequest url) >>= getResponseBody

data OTPError = OTPError String deriving (Eq, Show)

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
                                   legs :: [OTPLeg] } deriving (Eq, Show)

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

type ItineraryCache = Map.Map OTPPlanRequest OTPItineraryOrNot

emptyCache :: ItineraryCache
emptyCache = Map.empty

getFastestItineraryCaching :: OTPImpl s => s -> OTPPlanRequest -> ItineraryCache -> IO (OTPItineraryOrNot, ItineraryCache)
getFastestItineraryCaching otp request iCache =
  case Map.lookup request iCache of
    Just hit -> do
--      putStrLn ("Cache hit for " ++ show request ++ "!")
      return (hit, iCache)
    Nothing -> do
--      putStrLn ("Cache miss for " ++ show request ++ " with " ++ show (Map.size iCache) ++ " elements in cache.")
      response <- getFastestItinerary otp request
      return (response, Map.insert request response iCache :: ItineraryCache)


data OTPLeg = OTPLeg { lStartTime :: UTCTime,
                       lEndTime :: UTCTime,
                       lFrom :: OTPPlace,
                       lTo :: OTPPlace,
                       lTripID :: Maybe String,
                       lTripBlockID :: Maybe String,
                       lHeadSign :: Maybe String,
                       lRoute :: String,
                       lMode :: String} deriving (Eq)

instance FromJSON OTPLeg where
  parseJSON (Object v) =
    OTPLeg <$> (convertMillis <$> (v .: pack "startTime"))
           <*> (convertMillis <$> (v .: pack "endTime"))
           <*> v .: pack "from"
           <*> v .: pack "to"
           <*> v .:? pack "tripId"
           <*> v .:? pack "tripBlockId"
           <*> v .:? pack "headsign"
           <*> v .: pack "route"
           <*> v .: pack "mode"
  parseJSON _ = mzero

instance Show OTPLeg where
  show leg = showLeg cheapUTC leg

cheapUTC :: TimeZoneSeries
cheapUTC = TimeZoneSeries (TimeZone 0 False "UTC") []

showLeg :: TimeZoneSeries -> OTPLeg -> String
showLeg tz leg =
  let commonText = " from " ++ (pName $ lFrom leg) ++ "\n" ++ arrivalText
      railText = "Take " ++ lRoute leg ++ " train " ++ (fromMaybe "[numberless]" (lTripBlockID leg))
      walkText = "Start walking toward " ++ (pName $ lTo leg)
      timestamp = (displayTime tz $ lStartTime leg) ++ ": "
      arrivalText = (displayTime tz $ lEndTime leg) ++ ": Arrive at " ++ (pName $ lTo leg)
  in case (lMode leg) of
    "WALK" -> timestamp ++ walkText ++ commonText
    "RAIL" -> timestamp ++ railText ++ commonText
    "SUBWAY" -> timestamp ++ railText ++ commonText
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

showDeparture :: TimeZoneSeries -> OTPLeg -> String
showDeparture tz leg =
  let commonText = "From " ++ (pName $ lFrom leg) ++ ", "
      railText = "take train " ++ trainName leg ++ " on " ++ routeDesc leg
      walkText = "start walking toward " ++ (pName $ lTo leg)
      timestamp = timeHeading tz leg
  in case (lMode leg) of
    "WALK" -> timestamp ++ commonText ++ walkText
    "RAIL" -> timestamp ++ commonText ++ railText
    "TRAM" -> timestamp ++ commonText ++ railText
    "SUBWAY" -> timestamp ++ commonText ++ railText
    _      -> ("What the shit is " ++ lMode leg)

showArrival :: TimeZoneSeries -> OTPLeg -> String
showArrival tz leg = (displayTime tz $ lEndTime leg) ++ ": Arrive at " ++ (pName $ lTo leg) ++ "."

showLayover :: OTPLeg -> OTPLeg -> String
showLayover l1 l2 = " Kill " ++ (show minutes) ++ " minutes at " ++ (pName $ lFrom l2) ++ "."
  where minutes = quot (ceiling $ diffUTCTime (lStartTime l2) (lEndTime l1)) 60

timeHeading :: TimeZoneSeries -> OTPLeg -> String
timeHeading tz leg = (displayTime tz $ lStartTime leg) ++ ": "

prettyRouteName :: OTPLeg -> String
prettyRouteName leg = let
  routeNameFromOTP = lRoute leg
  lastFive = take 5 $ reverse $ routeNameFromOTP
  endsInLine = map toLower lastFive == " line"
  lineSuffix = if endsInLine then "" else " line"
  in routeNameFromOTP ++ lineSuffix

routeDesc :: OTPLeg -> String
routeDesc leg = "the " ++ prettyRouteName leg ++ " with head sign " ++ (fromMaybe "[no head sign]" $ lHeadSign leg)

showLegs :: TimeZoneSeries -> [OTPLeg] -> [String]
showLegs _ [] = []
showLegs tz (l:[]) = [showDeparture tz l, showArrival tz l]
showLegs tz (l1:(l2:xs))
-- if l1 is rail and l2 is rail and they are the same train
-- let's say the departure, no arrival, and "stay on the train" then recurse xs
  | sameTrain l1 l2 = [showDeparture tz l1, timeHeading tz l2 ++ "Stay on the train as it becomes the " ++ routeDesc l2, showArrival tz l2] ++ showLegs tz xs
-- if l1 is rail and l2 is rail and they are different trains, let's show
-- l1, the arrival+killing, and recurse l2:xs
  | isRail l1 && isRail l2 = [showDeparture tz l1, showArrival tz l1 ++ showLayover l1 l2] ++ showLegs tz (l2:xs)
-- if l1 leg is rail and l2 is walking, let's give the l1 departure and arrival
  | isRail l1 && (lMode l2) == "WALK" = [showDeparture tz l1, showArrival tz l1] ++ showLegs tz (l2:xs)
-- if l1 is walking, let's give the walking then the arrival+killing
-- then recurse on l2:xs
  | (lMode l1 == "WALK") = [showDeparture tz l1, showArrival tz l1 ++ showLayover l1 l2] ++ showLegs tz (l2:xs)
  | otherwise = (show l1 ++ " AND THIS IS THE ERROR CASE"):(showLegs tz (l2:xs))
  where isRail leg = (lMode leg == "RAIL") || (lMode leg == "SUBWAY") || (lMode leg == "TRAM")

data OTPPlace = OTPPlace { pName :: String } deriving (Eq, Show)

instance FromJSON OTPPlace where
  parseJSON (Object v) =
    OTPPlace <$> v .: pack "name"
  parseJSON _ = mzero

data OTPStopTimesList = OTPStopTimesList {
  pattern :: Object,
  times :: [OTPStopTime]
} deriving (Eq, Show)
  
instance FromJSON OTPStopTimesList where
  parseJSON (Object v) =
    OTPStopTimesList <$> v .: pack "pattern"
                     <*> v .: pack "times"
  parseJSON _ = mzero

data OTPStopTime = OTPStopTime {
  stopDepartureTime :: UTCTime,
  stopID :: String,
  tripID :: String
} deriving (Eq, Show)

instance FromJSON OTPStopTime where
  parseJSON (Object v) =
    OTPStopTime <$> (posixSecondsToUTCTime <$> ((+) <$> (v .: pack "serviceDay") <*> (v .: pack "scheduledDeparture")))
                <*> v .: pack "stopId"
                <*> v .: pack "tripId"
  parseJSON _ = mzero

-- >>> data[0]['times'][0]
-- {u'scheduledArrival': 27900, u'realtimeArrival': 27900, u'blockId': u'0871', u'tripId': u'1:1768', u'stopIndex': 23, u'departureDelay': 0, u'realtime': False, u'realtimeDeparture': 27900, u'headsign': u'HACKETTSTOWN', u'timepoint': True, u'serviceDay': 1477540800, u'stopId': u'1:54', u'stopCount': 24, u'realtimeState': u'SCHEDULED', u'scheduledDeparture': 27900, u'arrivalDelay': 0}

parseStopTimesResponse :: String -> [OTPStopTimesList]
parseStopTimesResponse text =
  let bs = packChars text
      decoded = eitherDecode bs
  in case decoded of
    Left msg -> error ("Decoding failed because " ++ msg ++ " when trying to decode " ++ text)
    Right obj -> obj

parseStopTimes :: String -> [OTPStopTime]
parseStopTimes = concat . map times . parseStopTimesResponse

-- We just need a stop ID and a date
data OTPStopTimesRequest = OTPStopTimesRequest {
  reqStopCode :: String,
  reqDate :: String -- YYYYMMDD
} deriving Show

queryURLStopTimes :: String -> OTPStopTimesRequest -> String
queryURLStopTimes routerAddress (OTPStopTimesRequest stopCode date) =
  routerAddress ++ "index/stops/" ++ stopCode ++ "/stoptimes/" ++ date

getStopTimesHTTP :: OTPHTTPServer -> OTPStopTimesRequest -> IO String
getStopTimesHTTP (OTPHTTPServer routerAddress) request =
  let url = queryURLStopTimes routerAddress request
  in simpleHTTP (getRequest url) >>= getResponseBody

data OTPStop = OTPStop { stopCode :: String, stopName :: String } deriving (Eq, Ord, Show)
instance FromJSON OTPStop where
  parseJSON (Object v) =
    OTPStop <$> v .: pack "id"
            <*> v .: pack "name"
  parseJSON _ = mzero

parseStopsResponse :: String -> [OTPStop]
parseStopsResponse text =
  let bs = packChars text
      decoded = eitherDecode bs
  in case decoded of
    Left msg -> error ("Decoding failed because " ++ msg ++ " when trying to decode " ++ text)
    Right obj -> obj

getStopsHTTP :: OTPHTTPServer -> IO String
getStopsHTTP (OTPHTTPServer routerAddress) =
  let url = routerAddress ++ "index/stops/"
  in simpleHTTP (getRequest url) >>= getResponseBody


