-- A fractal consisting of circles and lines which looks a bit like
--      the workings of a clock.
module Main where

import qualified Data.Time as Time
import Graphics.Gloss (
  Display (InWindow),
  Picture (
    Circle,
    Color,
    Line,
    Pictures,
    Scale
  ),
  animate,
  black,
  blue,
  green,
  white,
 )
import Graphics.Gloss.Data.Color (red)

main :: IO ()
main = do
  nowUTC <- Time.getCurrentTime
  tz <- Time.getCurrentTimeZone
  let now = Time.utcToLocalTime tz nowUTC
  animate
    (InWindow "Clock" (600, 600) (20, 20))
    black
    (frame now notSmooth)

smooth :: Real a => a -> Time.NominalDiffTime
smooth = Time.secondsToNominalDiffTime . fromRational . toRational

notSmooth :: RealFrac a => a -> Time.NominalDiffTime
notSmooth = Time.secondsToNominalDiffTime . fromInteger . round

-- Build the fractal, scale it so it fits in the window
-- and rotate the whole thing as time moves on.
frame :: Time.LocalTime -> (Float -> Time.NominalDiffTime) -> Float -> Picture
frame initial secFunc t =
  let
    sec = secFunc $ t
    newTime = Time.addLocalTime sec initial
   in
    Scale 120 120 $
      Pictures [Color white $ Circle 2.1, drawArrows (Time.localTimeOfDay newTime)]

drawArrows :: Time.TimeOfDay -> Picture
drawArrows t = arrows
 where
  h = fromIntegral (Time.todHour t) * (1 / 24) * 2 * pi
  m = fromIntegral (Time.todMin t) * (1 / 60) * 2 * pi
  s = fromRational (toRational (Time.todSec t)) * (1 / 60) * 2 * pi
  -- join each iteration to the origin with some lines.
  arrows =
    Pictures
      [ Color red $ Scale 0.8 0.8 $ Line [(0, 0), (sin h, cos h)]
      , Color green $ Scale 1.5 1.5 $ Line [(0, 0), (sin m, cos m)]
      , Color blue $ Scale 2 2 $ Line [(0, 0), (sin s, cos s)]
      ]