module Main where
  import qualified Data.Set as Set
  import WhitherTSP
  import WhitherOTP (defaultOTP)
  import Data.Time.Clock (addUTCTime, UTCTime(..))
  import Data.Time.Calendar (fromGregorian)

-- 17219 is the destination for Ocean Beach
-- 15223 is the source gonna try to just use the destination? Nope can't do that
-- it won't let me calculate directions from 1:15223. Maybe a walking penalty?
-- route IDs for the muni metro are all just the letters

{-
Let's write out our process here.
We start at either Ocean Beach or Sunnydale.
There are two distinct graphs to walk - the Van Ness - Balboa Park one, and the one around 3rd St.
Once you enter one of those, you don't leave until you've finished it.
Tasks you have to complete in the Balboa Park segment:
- all three legs of the triangle between Duboce/Church, Van Ness, and Market/Church. (you will have to do one twice)
- the J from Market/Church to Balboa Park
- the K from St Francis Circle to Balboa Park (you will probably have to do this twice)
- the M from St Francis Circle to Balboa Park
- the K OR M from Market/Church to St Francis Circle
You'll enter that segment at Duboce/Church if you are coming from Ocean Beach.
You'll enter it at Van Ness if you are coming from Sunnydale.
Tasks you have to complete in the Third Street segment:
- The N between Montgomery and Caltrain.
- The T between Montgomery and Chinatown (twice)
- The T between Montgomery and Caltrain (probably twice)
It kind of feels like the fastest way coming from Balboa Park would be JKMN to
N to Caltrain, then T to Chinatown, then T the rest of the way.
From Sunnydale, T to Chinatown, back to Caltrain, N to the rest of the trip.

-}


  allStations = map (\(s,c) -> Station s c) [ 
      ("Ocean Beach outbound", "1:17219")
    , ("Ocean Beach inbound", "1:15223")
    , ("Van Ness inbound", "1:15419") -- K/M/N transfers
    , ("Van Ness outbound", "1:16996")
    , ("St. Francis Circle inbound", "1:17109") -- K/M transfers
    , ("St. Francis Circle outbound", "1:16503")
    , ("Balboa Park JK inbound", "1:15418")
    , ("Balboa Park M outbound", "1:16262")
    , ("Sunnydale inbound", "1:17398")
    , ("Chinatown", "1:17876")
    , ("Caltrain T inbound", "1:17166")
    , ("Caltrain T outbound", "1:17397")
    , ("Caltrain N inbound", "1:15240")
    ]

  getStation :: String -> Station
  getStation stationName = let
    candidates = filter (\s -> name s == stationName) allStations
    in (case candidates of
          (x:xs) -> x
          []     -> error (stationName ++ " is misspelled. This getStation thing is moronic."))

  -- 2300 UTC = 1600 PDT
  myStartTime = UTCTime (fromGregorian 2023 09 01) (15*60*60 + 0*60)
  -- BART is small so this does not matter.
  myDeadline = addUTCTime (60 * 60 * 25) myStartTime

  instructions = [
      OTPHop (getStation "Sunnydale inbound") []
    , OTPHop (getStation "Chinatown") []
    , OTPHop (getStation "Caltrain T outbound") []
    , ForcedTransfer (getStation "Caltrain N inbound") 60
    , OTPHop (getStation "Van Ness outbound") [("preferredRoutes", "1:N")]
--    , OTPHop (getStation "Balboa Park JK inbound") [("preferredRoutes", "1:J"), ("bannedRoutes", "1:K,1:M")]
    , OTPHop (getStation "Balboa Park JK inbound") []
    , ForcedTransfer (getStation "Balboa Park M outbound") 60
    , OTPHop (getStation "St. Francis Circle inbound") [("preferredRoutes", "1:M"), ("bannedRoutes", "1:J,1:K")]
    , ForcedTransfer (getStation "St. Francis Circle outbound") 60
    , OTPHop (getStation "Balboa Park JK inbound") [] -- can do this via any mode
    , OTPHop (getStation "Van Ness inbound") [("preferredRoutes", "1:J"), ("bannedRoutes", "1:K,1:M")]
    , ForcedTransfer (getStation "Van Ness outbound") 60
    , OTPHop (getStation "Ocean Beach outbound") [("preferredRoutes", "1:N")]
    ]

  main :: IO()
  main = do
    followInstructions defaultOTP instructions myStartTime myDeadline "/usr/share/zoneinfo/US/Pacific"
