module WhitherOTP where

import Control.Monad (liftM, mzero)
import Data.Aeson ((.:), (.:?), eitherDecode, FromJSON, parseJSON)
import Data.Aeson.Types (Object, Value(..))
import Data.ByteString.Lazy.Internal (packChars)
import Data.List (intercalate, minimumBy)
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Data.Time.Clock (UTCTime(..))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (TimeZone(..), utcToZonedTime)
import Network.HTTP (getRequest, getResponseBody, simpleHTTP)

failEDT = TimeZone (-4 * 60) True "failEDT"
failEST = TimeZone (-5 * 60) True "failEST"

convertMillis :: Int -> UTCTime
convertMillis us = posixSecondsToUTCTime (fromIntegral $ quot us 1000)

queryTime :: UTCTime -> String
queryTime utc = formatTime defaultTimeLocale "&date=%Y%m%d&time=%H:%M" (utcToZonedTime failEST utc)

displayTime :: UTCTime -> String
displayTime utc = formatTime defaultTimeLocale "%Y%m%d-%H%M %Z" (utcToZonedTime failEST utc)

data OTPHTTPServer = OTPHTTPServer String
routerAddress = "http://localhost:8080/otp/routers/default/"

defaultOTP = OTPHTTPServer routerAddress

queryURL :: String -> String -> String -> UTCTime -> [String] -> String
queryURL routerAddress fromCode toCode travelTime planFlags
  | otherwise = url
  where url = routerAddress ++ "plan?minTransferTime=180&fromPlace=" ++ fromCode ++ "&toPlace=" ++ toCode ++ queryTime travelTime ++ extraParams
        extraParams = "&" ++ intercalate "&" planFlags

getRouteHTTP :: OTPHTTPServer -> String -> String -> UTCTime -> [String] -> IO String
getRouteHTTP (OTPHTTPServer routerAddress) fromStation toStation travelTime planFlags =
  let url = queryURL routerAddress fromStation toStation travelTime planFlags
  in simpleHTTP (getRequest url) >>= getResponseBody

class OTPImpl a where
  getRouteJSON :: a -> String -> String -> UTCTime -> [String] -> IO String

instance OTPImpl OTPHTTPServer where
  getRouteJSON server = getRouteHTTP server

data StaticFileReader = StaticFileReader
instance OTPImpl StaticFileReader where
  getRouteJSON _ _ _ _ _ = readFile "route.json"

sampleResponse :: IO OTPResponse
sampleResponse = liftM parseResponse $ readFile "route.json"

parseResponse :: String -> OTPResponse
parseResponse text =
  let bs = packChars text
      decoded = eitherDecode bs
  in case decoded of
    Left msg -> error ("Decoding failed because " ++ msg ++ " when trying to decode " ++ text)
    Right obj -> obj

data OTPResponse =
  OTPResponse { plan :: OTPTripPlan,
                requestParameters :: Object } deriving Show

instance FromJSON OTPResponse where
 parseJSON (Object v) =
    OTPResponse <$> v .: pack "plan"
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

getFastestItinerary :: OTPImpl s => s -> String -> String -> UTCTime -> [String] -> IO OTPItinerary
getFastestItinerary a b c d e = liftM fastestItinerary $ getPlan a b c d e

data OTPLeg = OTPLeg { lStartTime :: UTCTime,
                       lEndTime :: UTCTime,
                       lFrom :: OTPPlace,
                       lTo :: OTPPlace,
                       lTripBlockID :: Maybe String,
                       lRoute :: String,
                       lMode :: String}

instance FromJSON OTPLeg where
  parseJSON (Object v) =
    OTPLeg <$> (convertMillis <$> (v .: pack "startTime"))
           <*> (convertMillis <$> (v .: pack "endTime"))
           <*> v .: pack "from"
           <*> v .: pack "to"
           <*> v .:? pack "tripBlockId"
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

data OTPPlace = OTPPlace { pName :: String } deriving Show

instance FromJSON OTPPlace where
  parseJSON (Object v) =
    OTPPlace <$> v .: pack "name"
  parseJSON _ = mzero

getPlan :: OTPImpl s => s -> String -> String -> UTCTime -> [String] -> IO OTPTripPlan
getPlan otp from to travelTime planFlags = liftM (plan . parseResponse) $ getRouteJSON otp from to travelTime planFlags

