module Main where

import Config (AnimationType (..), Config (..), readConfig)
import qualified Data.Time as Time
import Graphics.Gloss (
  Display (InWindow),
  Picture,
  animate,
  black,
  blue,
  circle,
  color,
  green,
  line,
  pictures,
  scale,
  text,
  translate,
  white,
 )
import Graphics.Gloss.Data.Color (red)

data ClockAnimationHandle = ClockAnimationHandle
  { hours :: Time.TimeOfDay -> Float
  , minutes :: Time.TimeOfDay -> Float
  , seconds :: Time.TimeOfDay -> Float
  }

main :: IO ()
main = do
  tz <- Time.getCurrentTimeZone
  nowUTC <- Time.getCurrentTime
  conf <- readConfig
  let now = Time.utcToLocalTime tz nowUTC
  let animHandle = case animType conf of
        Smooth ->
          ClockAnimationHandle
            { hours = \t -> fromRational (toRational (fromIntegral ((Time.todHour t `mod` 12) * 3600 + Time.todMin t * 60) + Time.todSec t) / 3600)
            , minutes = \t -> fromRational (toRational (fromIntegral (Time.todMin t) * 60 + Time.todSec t) / 60)
            , seconds = fromRational . toRational . Time.todSec
            }
        Mixed ->
          ClockAnimationHandle
            { hours = fromIntegral . (`mod` 12) . Time.todHour
            , minutes = fromIntegral . Time.todMin
            , seconds = fromRational . toRational . Time.todSec
            }
        Digital ->
          ClockAnimationHandle
            { hours = fromIntegral . (`mod` 12) . Time.todHour
            , minutes = fromIntegral . Time.todMin
            , seconds = fromInteger . floor . Time.todSec
            }
  animate
    (InWindow "Clock" (600, 600) (20, 20))
    black
    (frame animHandle now)

-- Build the fractal, scale it so it fits in the window
-- and rotate the whole thing as time moves on.
frame :: ClockAnimationHandle -> Time.LocalTime -> Float -> Picture
frame ClockAnimationHandle {hours = h', minutes = m', seconds = s'} initial timeInSec =
  let
    sec = Time.secondsToNominalDiffTime . fromRational . toRational $ timeInSec
    t = Time.localTimeOfDay . Time.addLocalTime sec $ initial
    h = h' t / 12 * 2 * pi
    m = m' t / 60 * 2 * pi
    s = s' t / 60 * 2 * pi
   in
    scale 240 240 $
      pictures [back, drawArrows h m s, time (Time.todHour t) (Time.todMin t) ((`mod` 60) . floor . Time.todSec $ t)]

time :: Int -> Int -> Int -> Picture
time h m s = color white . translate (-0.25) (-0.25) . scale 0.001 0.001 . text $ (padLeft '0' 2 . show $ h) ++ ":" ++ (padLeft '0' 2 . show $ m) ++ ":" ++ (padLeft '0' 2 . show $ s)

{-# INLINE padLeft #-}
padLeft :: a -> Int -> [a] -> [a]
padLeft c n xs = replicate (n - length xs) c ++ xs

back :: Picture
back = color white . pictures $ [circle 1, minuteLines, hourLines]
 where
  minuteLines = pictures [line [(minuteLength * sin h, minuteLength * cos h), (sin h, cos h)] | h <- (\x -> x / 60 * 2 * pi) . pred <$> [1 .. 60]]
  hourLines = pictures [line [(hourLength * sin h, hourLength * cos h), (sin h, cos h)] | h <- (\x -> x / 12 * 2 * pi) . pred <$> [1 .. 12]]

drawArrows :: Float -> Float -> Float -> Picture
drawArrows h m s =
  pictures
    [ color blue . scale minuteLength minuteLength . line $ [(0, 0), (sin s, cos s)]
    , color green . scale hourLength hourLength . line $ [(0, 0), (sin m, cos m)]
    , color red . scale 0.4 0.4 . line $ [(0, 0), (sin h, cos h)]
    ]

hourLength :: Float
hourLength = 0.8

minuteLength :: Float
minuteLength = 0.95