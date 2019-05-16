{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Static
-- Copyright   :  (c) 2015 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jeffrey.rosenbluth@gmail.com
--
-- A small DSL for creating HTML5 Canvas with haskell.
--
-- <<http://i.imgur.com/HGjSpJ6.png>>
--
-- > module Main where
-- >
-- > import Graphics.Static
-- > import Graphics.Static.ColorNames
-- >
-- > text :: CanvasFree ()
-- > text = do
-- >   font "italic 60pt Calibri"
-- >   lineWidth 6
-- >   strokeStyle blue
-- >   fillStyle goldenrod
-- >   textBaseline TextBaselineMiddle
-- >   strokeText "Hello" 25 100
-- >   fillText "Hello static-canvas!" 25 100
-- >
-- > main :: IO ()
-- > main = writeCanvasDoc "example.html" 650 300 text
--
-- The static-canvas API shadows the actual Javascript API, and thus the
-- best place to look for a more detailed definition of the canvas functions
-- including the definitions of it's aruments see <http://www.w3.org/TR/2dcontext/>.
-------------------------------------------------------------------------------

module Graphics.Static
  (
   -- * Building and Writing
    evalScript
  , buildScript
  , buildScript'
  , buildDoc
  , writeCanvasScript
  , writeCanvasScript'
  , writeCanvasDoc
   -- * HTML5 Canvas API
  , MFC
   -- ** Paths
  , beginPath
  , closePath
  , fill
  , stroke
  , clip
  , moveTo
  , lineTo
  , quadraticCurveTo
  , bezierCurveTo
  , arcTo
  , arc
  , rect
  , ellipse
   -- ** Line styles
  , lineWidth
  , lineCap
  , lineJoin
  , lineDash
  , miterLimit
  , LineCapStyle(..)
  , LineJoinStyle(..)
   -- ** Colors, styles and shadows
  , strokeStyle
  , fillStyle
  , shadowOffsetX
  , shadowOffsetY
  , shadowBlur
  , shadowColor
  , createLinearGradient
  , createRadialGradient
  , addColorStop
  , Gradient(..)
  , createPattern
  , RepeatStyle(..)
  , Color(..)
  , Style(..)
   -- ** Color utilities
  , rgb
  , rgba
   -- ** Text
  , font
  , textAlign
  , textBaseline
  , fillText
  , strokeText
  , TextAlignStyle(..)
  , TextBaselineStyle(..)
   -- ** Rectangles
  , clearRect
  , fillRect
  , strokeRect
   -- ** Context
  , save
  , restore
   -- ** Transformations
  , scale
  , rotate
  , translate
  , transform
  , setTransform
   -- ** Images
  , drawImageAt
  , drawImageSize
  , drawImageCrop
  , newImage
  , onImageLoad
   -- ** Compositing
  , globalAlpha
  , globalCompositeOperation
  , CompositeOperation(..)
  ) where

import Control.Monad.Free.Class
import Data.Monoid
import Prelude                     hiding (writeFile)
import Data.Text                   (Text)
import Data.Text.Lazy.Builder      (Builder, toLazyText, fromText)
import Data.Text.Lazy.IO           (writeFile)
import Graphics.Static.Interpreter
import Graphics.Static.Javascript
import Graphics.Static.Types

-------------------------------------------------------------------------------
-- Building and writing
-------------------------------------------------------------------------------

-- | Write a canvas document to a file.
writeCanvasDoc :: FilePath -> Int -> Int -> MFC () -> IO ()
writeCanvasDoc path w h canvas =
  writeFile path (toLazyText $ buildDoc w h canvas)

-- | Write a canvas script element to a file.
writeCanvasScript :: FilePath -> Int -> Int -> MFC () -> IO ()
writeCanvasScript path w h = writeCanvasScript' path w h ""

-- | More general version of 'writeCanvasScript', that takes a unique identifier
--   as an additional parameter so that multiple canvas elements can be included
--   in the same html document.
writeCanvasScript' :: FilePath -> Int -> Int -> Text -> MFC () -> IO ()
writeCanvasScript' path w h uniqId canvas =
  writeFile path (toLazyText $ buildScript' w h uniqId canvas)

-- | Create a 'Builder' representing a canvas document.
buildDoc :: Int -> Int -> MFC () -> Builder
buildDoc w h canvas
  =  "<!DOCTYPE HTML><html><body>"
  <> (buildScript w h canvas)
  <> "</body></html>"

-- | Create a 'Builder' representing a canvas script.
buildScript :: Int -> Int -> MFC () -> Builder
buildScript w h = buildScript' w h ""

-- | More general version of 'buildScript', that takes a unique identifier
--   as an additional parameter so that multiple canvas elements can be included
--   in the same html document.
buildScript' :: Int -> Int -> Text -> MFC () -> Builder
buildScript' w h uniqId canvas
  =  "<canvas id=\"" <> uId <> "StaticCanvas\" width=\"" <> jsInt w
  <> "\" height=\"" <> jsInt h <> "\"></canvas>"
  <> "<script>"
  <> "(function () {"
  <> "var canvas = document.getElementById('"<> uId <> "StaticCanvas');"
  <> "var " <> uId <> "Ctx = canvas.getContext('2d');"
  <> (evalScript uniqId canvas)
  <> "}());"
  <> "</script>"
  where
    uId = fromText uniqId

-------------------------------------------------------------------------------
-- Color utilities
-------------------------------------------------------------------------------

rgb :: Int -> Int -> Int -> Style
rgb r g b = ColorStyle (RGB r g b)

rgba :: Int -> Int -> Int -> Double -> Style
rgba r g b a = ColorStyle (RGBA r g b a)

-------------------------------------------------------------------------------
-- The DSL
-------------------------------------------------------------------------------
addColorStop :: Double -> Color -> Style -> MFC ()
addColorStop a1 a2 a3 = liftF $ AddColorStop a1 a2 a3 ()

arc :: Double -> Double -> Double -> Double -> Double -> Bool -> MFC ()
arc a1 a2 a3 a4 a5 a6 = liftF $ Arc a1 a2 a3 a4 a5 a6 ()

arcTo :: Double -> Double -> Double -> Double -> Double -> MFC ()
arcTo a1 a2 a3 a4 a5 = liftF $ ArcTo a1 a2 a3 a4 a5 ()

beginPath :: MFC ()
beginPath = liftF $ BeginPath ()

-- | Cubic Bezier curve.
bezierCurveTo :: Double -> Double -> Double -> Double -> Double -> Double -> MFC ()
bezierCurveTo a1 a2 a3 a4 a5 a6 = liftF $ BezierCurveTo a1 a2 a3 a4 a5 a6 ()

clearRect :: Double -> Double -> Double -> Double -> MFC ()
clearRect a1 a2 a3 a4 = liftF $ ClearRect a1 a2 a3 a4 ()

clip :: MFC ()
clip = liftF $ Clip ()

closePath :: MFC ()
closePath = liftF $ ClosePath ()

createLinearGradient :: Double -> Double -> Double -> Double -> MFC Style
createLinearGradient a1 a2 a3 a4 = liftF $ CreateLinearGradient a1 a2 a3 a4 id

createPattern :: Int -> RepeatStyle -> MFC Style
createPattern a1 a2 = liftF $ CreatePattern a1 a2 id

createRadialGradient :: Double -> Double -> Double -> Double -> Double -> Double -> MFC Style
createRadialGradient a1 a2 a3 a4 a5 a6 = liftF $ CreateRadialGradient a1 a2 a3 a4 a5 a6 id

drawImageAt :: Int -> Double -> Double -> MFC ()
drawImageAt a1 a2 a3 = liftF $ DrawImageAt a1 a2 a3 ()

drawImageSize :: Int -> Double -> Double -> Double -> Double -> MFC ()
drawImageSize a1 a2 a3 a4 a5 = liftF $ DrawImageSize a1 a2 a3 a4 a5 ()

drawImageCrop :: Int -> Double -> Double -> Double -> Double -> Double
                     -> Double -> Double -> Double -> MFC ()
drawImageCrop a1 a2 a3 a4 a5 a6 a7 a8 a9
  = liftF $ DrawImageCrop a1 a2 a3 a4 a5 a6 a7 a8 a9 ()

ellipse :: Double -> Double -> Double -> Double -> Double -> Double -> Double
        -> Bool -> MFC ()
ellipse a1 a2 a3 a4 a5 a6 a7 a8 = liftF $ Ellipse a1 a2 a3 a4 a5 a6 a7 a8 ()

fill :: MFC ()
fill = liftF $ Fill ()

fillRect :: Double -> Double -> Double -> Double -> MFC ()
fillRect a1 a2 a3 a4 = liftF $ FillRect a1 a2 a3 a4 ()

fillStyle :: Style -> MFC ()
fillStyle a1 = liftF $ FillStyle a1 ()

fillText :: Text -> Double -> Double -> MFC ()
fillText a1 a2 a3 = liftF $ FillText a1 a2 a3 ()

font :: Text -> MFC ()
font a1 = liftF $ Font a1 ()

globalAlpha :: Double -> MFC ()
globalAlpha a1 = liftF $ GlobalAlpha a1 ()

globalCompositeOperation :: CompositeOperation -> MFC ()
globalCompositeOperation a1 = liftF $ GlobalCompositeOperation a1 ()

lineCap :: LineCapStyle -> MFC ()
lineCap a1 = liftF $ LineCap a1 ()

lineDash :: [Double] -> MFC ()
lineDash ds = liftF $ LineDash ds ()

lineJoin :: LineJoinStyle -> MFC ()
lineJoin a1 = liftF $ LineJoin a1 ()

lineTo :: Double -> Double -> MFC ()
lineTo a1 a2 = liftF $ LineTo a1 a2 ()

-- | Set the line width.
lineWidth :: Double -> MFC ()
lineWidth a1 = liftF $ LineWidth a1 ()

miterLimit :: Double -> MFC ()
miterLimit a1 = liftF $ MiterLimit a1 ()

moveTo :: Double -> Double -> MFC ()
moveTo a1 a2 = liftF $ MoveTo a1 a2 ()

newImage :: Text -> MFC Int
newImage a1 = liftF $ NewImage a1 id

-- | Useful for commands that need to wait for an image to load before
--   being called. For example
--
--   > image = do
--   > img <- newImage "http://www.staticcanvas.com/picture.png"
--   > onImageLoad img (drawImageAt img 0 0)
onImageLoad :: Int -> MFC () -> MFC ()
onImageLoad a1 a2 = liftF $ OnImageLoad a1 a2 ()

-- | A quadratic bezier curve.
quadraticCurveTo :: Double -> Double -> Double -> Double -> MFC ()
quadraticCurveTo a1 a2 a3 a4 = liftF $ QuadraticCurveTo a1 a2 a3 a4 ()

rect :: Double -> Double -> Double -> Double -> MFC ()
rect a1 a2 a3 a4 = liftF $ Rect a1 a2 a3 a4 ()

-- | Pop the top state of the stack.
restore :: MFC ()
restore = liftF $ Restore ()

rotate :: Double -> MFC ()
rotate a1 = liftF $ Rotate a1 ()

-- | Push the current state onto the stack.
save :: MFC ()
save = liftF $ Save ()

scale :: Double -> Double -> MFC ()
scale a1 a2 = liftF $ Scale a1 a2 ()

setTransform :: Double -> Double -> Double -> Double -> Double -> Double -> MFC ()
setTransform a1 a2 a3 a4 a5 a6 = liftF $ SetTransform a1 a2 a3 a4 a5 a6 ()

shadowBlur :: Double -> MFC ()
shadowBlur a1 = liftF $ ShadowBlur a1 ()

shadowColor :: Color -> MFC ()
shadowColor a1 = liftF $ ShadowColor a1 ()

shadowOffsetX :: Double -> MFC ()
shadowOffsetX a1 = liftF $ ShadowOffsetX a1 ()

shadowOffsetY :: Double -> MFC ()
shadowOffsetY a1 = liftF $ ShadowOffsetY a1 ()

stroke :: MFC ()
stroke = liftF $ Stroke ()

strokeRect :: Double -> Double -> Double -> Double -> MFC ()
strokeRect a1 a2 a3 a4 = liftF $ StrokeRect a1 a2 a3 a4 ()

strokeStyle :: Style -> MFC ()
strokeStyle a1 = liftF $ StrokeStyle a1 ()

strokeText :: Text -> Double -> Double -> MFC ()
strokeText a1 a2 a3 = liftF $ StrokeText a1 a2 a3 ()

textAlign :: TextAlignStyle -> MFC ()
textAlign a1 = liftF $ TextAlign a1 ()

textBaseline :: TextBaselineStyle -> MFC ()
textBaseline a1 = liftF $ TextBaseline a1 ()

transform :: Double -> Double -> Double -> Double -> Double -> Double -> MFC ()
transform a1 a2 a3 a4 a5 a6 = liftF $ Transform a1 a2 a3 a4 a5 a6 ()

translate :: Double -> Double -> MFC ()
translate a1 a2 = liftF $ Translate a1 a2 ()
