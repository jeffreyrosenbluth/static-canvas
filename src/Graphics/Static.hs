{-# LANGUAGE OverloadedStrings #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Static
-- Copyright   :  (c) 2015 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jeffrey.rosenbluth@gmail.com
--
-- A small DSL for creating HTML5 Canvas.
--
-------------------------------------------------------------------------------

module Graphics.Static
  ( module Graphics.Static
  , Style(..)
  , Color(..)
  , Gradient(..)
  , LineCapStyle(..)
  , CanvasFree
  , evalScript
  ) where

import Control.Monad.Free          (liftF)
import Data.Monoid
import Prelude                     hiding (writeFile)
import Data.Text                   (Text)
import Data.Text.Lazy.Builder      (Builder, toLazyText)
import Data.Text.Lazy.IO           (writeFile) 
import Graphics.Static.Interpreter
import Graphics.Static.Javascript
import Graphics.Static.Types

writeCanvasDoc :: FilePath -> Int -> Int -> CanvasFree () -> IO ()
writeCanvasDoc path w h canvas = writeFile path (toLazyText $ buildDoc w h canvas)

writeCanvasScript :: FilePath -> Int -> Int -> CanvasFree () -> IO ()
writeCanvasScript path w h canvas = writeFile path (toLazyText $ buildScript w h canvas)

buildDoc :: Int -> Int -> CanvasFree () -> Builder
buildDoc w h canvas
  =  "<!DOCTYPE HTML><html><body>"
  <> (buildScript w h canvas)
  <> "</body></html>"

buildScript :: Int -> Int -> CanvasFree () -> Builder
buildScript w h canvas
  =  "<canvas id=\"theStaticCanvas\" width=\"" <> jsInt w
  <> "\" height=\"" <> jsInt h <> "\"></canvas>"
  <> "<script>"
  <> "var canvas = document.getElementById('theStaticCanvas');"
  <> "var ctx = canvas.getContext('2d');"
  <> (evalScript canvas)
  <> "</script>"

-------------------------------------------------------------------------------
-- The DSL
-------------------------------------------------------------------------------

addColorStop :: Int -> Color -> Style -> CanvasFree ()
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

font :: Text -> CanvasFree ()
font a1 = liftF $ Font a1 ()

globalAlpha :: Double -> CanvasFree ()
globalAlpha a1 = liftF $ GlobalAlpha a1 ()

globalCompositeOperation :: Text -> CanvasFree ()
globalCompositeOperation a1 = liftF $ GlobalCompositeOperation a1 ()

createLinearGradient :: Double -> Double -> Double -> Double -> CanvasFree Style
createLinearGradient a1 a2 a3 a4 = liftF $ LinearGradient a1 a2 a3 a4 id

lineCap :: LineCapStyle -> CanvasFree ()
lineCap a1 = liftF $ LineCap a1 ()

lineJoin :: LineJoinStyle -> CanvasFree ()
lineJoin a1 = liftF $ LineJoin a1 ()

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

createRadialGradient :: Double -> Double -> Double -> Double -> Double -> Double -> CanvasFree Style
createRadialGradient a1 a2 a3 a4 a5 a6 = liftF $ RadialGradient a1 a2 a3 a4 a5 a6 id

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

textAlign :: TextAlignStyle -> CanvasFree ()
textAlign a1 = liftF $ TextAlign a1 ()
                 
textBaseline :: TextBaselineStyle -> CanvasFree ()
textBaseline a1 = liftF $ TextBaseline a1 ()

transform :: Double -> Double -> Double -> Double -> Double -> Double -> CanvasFree ()
transform a1 a2 a3 a4 a5 a6 = liftF $ Transform a1 a2 a3 a4 a5 a6 ()

translate :: Double -> Double -> CanvasFree ()
translate a1 a2 = liftF $ Translate a1 a2 ()
