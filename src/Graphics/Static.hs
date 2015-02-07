-------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Static
-- Copyright   :  (c) 2015 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jeffrey.rosenbluth@gmail.com
--
-- DSL for creating HTML5 Canvas.
--
-------------------------------------------------------------------------------

module Graphics.Static
  ( module Graphics.Static
  , Style(..)
  , Color(..)
  , Gradient(..)
  , evalScript
  ) where

import Data.Text                   (Text)
import Graphics.Static.Internal
import Control.Monad.Free          (liftF)

addColorStop :: Double -> Color -> Int -> CanvasFree ()
addColorStop a1 a2 a3 = liftF $ AddColorStop a1 a2 a3 ()

arc :: Double -> Double -> Double -> Double -> Double -> Bool -> CanvasFree ()
arc a1 a2 a3 a4 a5 a6 = liftF $ Arc a1 a2 a3 a4 a5 a6 ()

arcTo :: Double -> Double -> Double -> Double -> Double -> CanvasFree ()
arcTo a1 a2 a3 a4 a5 = liftF $ ArcTo a1 a2 a3 a4 a5 ()

beginPath :: CanvasFree ()
beginPath = liftF $ BeginPath ()

bezierCurveTo :: Double -> Double -> Double -> Double -> Double -> Double -> CanvasFree ()
bezierCurveTo a1 a2 a3 a4 a5 a6 = liftF $ BezierCurveTo a1 a2 a3 a4 a5 a6 ()

clearRect :: Double -> Double -> Double -> Double -> CanvasFree ()
clearRect a1 a2 a3 a4 = liftF $ ClearRect a1 a2 a3 a4 ()

clip :: CanvasFree ()
clip = liftF $ Clip ()

closePath :: CanvasFree ()
closePath = liftF $ ClosePath ()

fill :: CanvasFree ()
fill = liftF $ Fill ()

fillRect :: Double -> Double -> Double -> Double -> CanvasFree ()
fillRect a1 a2 a3 a4 = liftF $ FillRect a1 a2 a3 a4 ()

fillStyle :: Style -> CanvasFree ()
fillStyle a1 = liftF $ FillStyle a1 ()

fillText :: Text -> Double -> Double -> CanvasFree ()
fillText a1 a2 a3 = liftF $ FillText a1 a2 a3 ()

globalAlpha :: Double -> CanvasFree ()
globalAlpha a1 = liftF $ GlobalAlpha a1 ()

globalCompositeOperation :: Text -> CanvasFree ()
globalCompositeOperation a1 = liftF $ GlobalCompositeOperation a1 ()

linearGradient :: Double -> Double -> Double -> Double -> CanvasFree Int
linearGradient a1 a2 a3 a4 = liftF $ LinearGradient a1 a2 a3 a4 id

lineTo :: Double -> Double -> CanvasFree ()
lineTo a1 a2 = liftF $ LineTo a1 a2 ()

lineWidth :: Double -> CanvasFree ()
lineWidth a1 = liftF $ LineWidth a1 ()

miterLimit :: Double -> CanvasFree ()
miterLimit a1 = liftF $ MiterLimit a1 ()

moveTo :: Double -> Double -> CanvasFree ()
moveTo a1 a2 = liftF $ MoveTo a1 a2 ()

quadraticCurveTo :: Double -> Double -> Double -> Double -> CanvasFree ()
quadraticCurveTo a1 a2 a3 a4 = liftF $ QuadraticCurveTo a1 a2 a3 a4 ()

radialGradient :: Double -> Double -> Double -> Double -> Double -> Double -> CanvasFree Int
radialGradient a1 a2 a3 a4 a5 a6 = liftF $ RadialGradient a1 a2 a3 a4 a5 a6 id

rect :: Double -> Double -> Double -> Double -> CanvasFree ()
rect a1 a2 a3 a4 = liftF $ Rect a1 a2 a3 a4 ()

restore :: CanvasFree ()
restore = liftF $ Restore ()

rotate :: Double -> CanvasFree ()
rotate a1 = liftF $ Rotate a1 ()

save :: CanvasFree ()
save = liftF $ Save ()

scale :: Double -> Double -> CanvasFree ()
scale a1 a2 = liftF $ Scale a1 a2 ()

setTransform :: Double -> Double -> Double -> Double -> Double -> Double -> CanvasFree ()
setTransform a1 a2 a3 a4 a5 a6 = liftF $ SetTransform a1 a2 a3 a4 a5 a6 ()

shadowBlur :: Double -> CanvasFree ()
shadowBlur a1 = liftF $ ShadowBlur a1 ()

shadowColor :: Color -> CanvasFree ()
shadowColor a1 = liftF $ ShadowColor a1 ()

shadowOffsetX :: Double -> CanvasFree ()
shadowOffsetX a1 = liftF $ ShadowOffsetX a1 ()

shadowOffsetY :: Double -> CanvasFree ()
shadowOffsetY a1 = liftF $ ShadowOffsetY a1 ()

stroke :: CanvasFree ()
stroke = liftF $ Stroke ()

strokeRect :: Double -> Double -> Double -> Double -> CanvasFree ()
strokeRect a1 a2 a3 a4 = liftF $ StrokeRect a1 a2 a3 a4 ()

strokeStyle :: Style -> CanvasFree ()
strokeStyle a1 = liftF $ StrokeStyle a1 ()

transform :: Double -> Double -> Double -> Double -> Double -> Double -> CanvasFree ()
transform a1 a2 a3 a4 a5 a6 = liftF $ Transform a1 a2 a3 a4 a5 a6 ()

translate :: Double -> Double -> CanvasFree ()
translate a1 a2 = liftF $ Translate a1 a2 ()
