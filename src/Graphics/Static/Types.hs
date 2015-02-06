{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Static.Types
-- Copyright   :  (c) 2015 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jeffrey.rosenbluth@gmail.com
--
-- DSL for creating HTML5 Canvas.
--
-------------------------------------------------------------------------------

module Graphics.Static.Types where

import           Control.Applicative
import           Control.Monad.Free          (Free(..), liftF)
import           Control.Monad.Free.Church   (F, fromF)
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Double.Conversion.Text (toFixed)
import           Data.Text                   (Text, pack)
import           Data.Text.Lazy.Builder      (Builder, fromText)

newtype Script a = Script {runScript :: (WriterT Builder (State Int) a)}
  deriving (Functor, Applicative, Monad, MonadWriter Builder, MonadState Int)

type CanvasFree = F Canvas

data Canvas r
  = -- AddColorStop
    Arc !Double !Double !Double !Double !Double !Bool r
  | ArcTo !Double !Double !Double !Double !Double r
  | BeginPath r
  | BezierCurveTo !Double !Double !Double !Double !Double !Double r
  | ClearRect !Double !Double !Double !Double r
  | Clip r
  | ClosePath r
  -- | CreatePattern
  -- | DrawImage2
  -- | DrawImage4
  -- | DrawImage8
  | Fill r
  | FillRect !Double !Double !Double !Double r
  -- | FillStyle
  | FillText !Text !Double !Double r
  -- | Font
  | GlobalAlpha !Double r
  | GlobalCompositeOperation !Text r
  | LinearGradient !Double !Double !Double !Double (Int -> r)
  -- | LineCap
  -- | LineJoin
  | LineTo !Double !Double r
  | LineWidth !Double r
  | MiterLimit !Double r
  | MoveTo !Double !Double r
  -- | NewImage
  -- | PutImageData2
  -- | PutImageData6
  | QuadraticCurveTo !Double !Double !Double !Double r
  | RadialGradient !Double !Double !Double !Double !Double !Double (Int -> r)
  | Rect !Double !Double !Double !Double r
  | Restore r
  | Rotate !Double r
  | Save r
  | Scale !Double !Double r
  | SetTransform !Double !Double !Double !Double !Double !Double r
  | ShadowBlur !Double r
  -- | ShadowColor
  | ShadowOffsetX !Double r
  | ShadowOffsetY !Double r
  | Stroke r
  | StrokeRect !Double !Double !Double !Double r
  -- | StrokeStyle
  | StrokeText !Text !Double !Double r
  -- | TextAlign
  -- | TextBaseLine
  | Transform !Double !Double !Double !Double !Double !Double r
  | Translate !Double !Double r
    deriving Functor


evalScript :: CanvasFree a -> Builder
evalScript c = (evalState . execWriterT . runScript . eval . fromF) c 0

record :: [Builder] -> Script ()
record = tell . mconcat

inc :: Script Int
inc = do
  n <- get
  put (n + 1)
  return n

eval :: Free Canvas a -> Script a
eval (Free (Arc a1 a2 a3 a4 a5 a6 c)) = do
  record ["ctx.arc("
         , jsDouble a1, comma, jsDouble a2, comma
         , jsDouble a3, comma, jsDouble a4, comma
         , jsDouble a5, comma, jsBool a6  , ");"]
  eval c
eval (Free (ArcTo a1 a2 a3 a4 a5 c)) = do
  record ["ctx.arcTo("
         , jsDouble a1, comma, jsDouble a2, comma
         , jsDouble a3, comma, jsDouble a4, comma
         , jsDouble a5, comma, ");"]
  eval c
eval (Free (BeginPath c)) = do
  tell "ctx.beginPath();"
  eval c
eval (Free (BezierCurveTo a1 a2 a3 a4 a5 a6 c)) = do
  record ["ctx.bezierCurveTo("
         , jsDouble a1, comma, jsDouble a2, comma
         , jsDouble a3, comma, jsDouble a4, comma
         , jsDouble a5, comma, jsDouble a6, ");"]
  eval c
eval (Free (ClearRect a1 a2 a3 a4 c)) = do
  record ["ctx.clearRect("
         , jsDouble a1, comma, jsDouble a2, comma
         , jsDouble a3, comma, jsDouble a4, ");"]
  eval c
eval (Free (Clip c)) = do
 tell "ctx.clip();"
 eval c
eval (Free (ClosePath c)) = do
  tell "ctx.closePath();"
  eval c
eval (Free (Fill c)) = do
  tell "ctx.fill();"
  eval c
eval (Free (FillRect a1 a2 a3 a4 c)) = do
  record ["ctx.fillRect("
         , jsDouble a1, comma, jsDouble a2, comma
         , jsDouble a3, comma, jsDouble a4, ");"]
  eval c
eval (Free (FillText a1 a2 a3 c)) = do
  record ["ctx.fillText(", fromText a1, comma
         , jsDouble a2, comma
         , jsDouble a3, ");"]
  eval c
eval (Free (GlobalAlpha a1 c)) = do
  record ["ctx.globalAlpha = (", jsDouble a1, ");"]
  eval c
eval (Free (GlobalCompositeOperation a1 c)) = do
  record ["ctx.globalCompositeOperation = (", fromText a1, ");"]
  eval c
eval (Free (LinearGradient a1 a2 a3 a4 k)) = do
  i <- inc
  record ["var gradient_", jsInt i, " = ctx.creatLinearGradient("
         , jsDouble a1, comma, jsDouble a2, comma
         , jsDouble a3, comma, jsDouble a4, ");"]
  eval (k i)
eval (Free (LineTo a1 a2 c)) = do
  record ["ctx.lineTo(", jsDouble a1, comma, jsDouble a2, ");"]
  eval c
eval (Free (LineWidth a1 c)) = do
  record ["ctx.lineWidth = (", jsDouble a1, ");"]
  eval c
eval (Free (MiterLimit a1 c)) = do
  record ["ctx.miterLimit = (", jsDouble a1, ");"]
  eval c
eval (Free (MoveTo a1 a2 c)) = do
  record ["ctx.moveTo(", jsDouble a1, comma, jsDouble a2, ");"]
  eval c
eval (Free (QuadraticCurveTo a1 a2 a3 a4 c)) = do
  record ["ctx.quadraticCurveTo("
         , jsDouble a1, comma, jsDouble a2, comma
         , jsDouble a3, comma, jsDouble a4, ");"]
  eval c
eval (Free (RadialGradient a1 a2 a3 a4 a5 a6 k)) = do
  i <- inc
  record ["var gradient_", jsInt i, " = ctx.creatRadialGradient("
         , jsDouble a1, comma, jsDouble a2, comma
         , jsDouble a3, comma, jsDouble a4, comma
         , jsDouble a5, comma, jsDouble a6, ");"]
  eval (k i)
eval (Free (Rect a1 a2 a3 a4 c)) = do
  record ["ctx.rect(", jsDouble a1, comma
                     , jsDouble a2, comma
                     , jsDouble a3, comma
                     , jsDouble a4, ");"]
  eval c
eval (Free (Restore c)) = do
  tell "ctx.restore();"
  eval c
eval (Free (Rotate a1 c)) = do
  record ["ctx.rotate(", jsDouble a1, ");"]
  eval c
eval (Free (Save c)) = do
  tell "ctx.save();"
  eval c
eval (Free (Scale a1 a2 c)) = do
  record ["ctx.scale(", jsDouble a1, comma, jsDouble a2, ");"]
  eval c
eval (Free (SetTransform a1 a2 a3 a4 a5 a6 c)) = do
  record ["ctx.setTransform("
         , jsDouble a1, comma, jsDouble a2, comma
         , jsDouble a3, comma, jsDouble a4, comma
         , jsDouble a5, comma, jsDouble a6, ");"]
  eval c
eval (Free (ShadowBlur a1 c)) = do
  record ["ctx.shadowBlur = (", jsDouble a1, ");"]
  eval c
eval (Free (ShadowOffsetX a1 c)) = do
  record ["ctx.shadowOffsetX = (", jsDouble a1, ");"]
  eval c
eval (Free (ShadowOffsetY a1 c)) = do
  record ["ctx.shadowOffsetY = (", jsDouble a1, ");"]
  eval c
eval (Free (Stroke c)) = do
  tell "ctx.stroke();"
  eval c
eval (Free (StrokeRect a1 a2 a3 a4 c)) = do
  record ["ctx.strokeRect("
         , jsDouble a1, comma, jsDouble a2, comma
         , jsDouble a3, comma, jsDouble a4, ");"]
  eval c
eval (Free (StrokeText a1 a2 a3 c)) =do
  record ["ctx.strokeText(", fromText a1, comma, jsDouble a2, comma, jsDouble a3, ");"]
  eval c
eval (Free (Transform a1 a2 a3 a4 a5 a6 c)) = do
  record ["ctx.transform("
         , jsDouble a1, comma, jsDouble a2, comma
         , jsDouble a3, comma, jsDouble a4, comma
         , jsDouble a5, comma, jsDouble a6, ");"]
  eval c
eval (Free (Translate a1 a2 c)) = do
  record ["translate(", jsDouble a1, comma, jsDouble a2, ");"]
  eval c
eval (Pure x) = return x

comma :: Builder
comma = comma

jsBool :: Bool -> Builder
jsBool True = "true"
jsBool False = "false"

jsInt :: Int -> Builder
jsInt = fromText . pack . show

jsDouble :: Double -> Builder
jsDouble = fromText . toFixed 4

-------------------------------------------------------------------------------

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

shadowOffsetX :: Double -> CanvasFree ()
shadowOffsetX a1 = liftF $ ShadowOffsetX a1 ()

shadowOffsetY :: Double -> CanvasFree ()
shadowOffsetY a1 = liftF $ ShadowOffsetY a1 ()

stroke :: CanvasFree ()
stroke = liftF $ Stroke ()

strokeRect :: Double -> Double -> Double -> Double -> CanvasFree ()
strokeRect a1 a2 a3 a4 = liftF $ StrokeRect a1 a2 a3 a4 ()

transform :: Double -> Double -> Double -> Double -> Double -> Double -> CanvasFree ()
transform a1 a2 a3 a4 a5 a6 = liftF $ Transform a1 a2 a3 a4 a5 a6 ()

translate :: Double -> Double -> CanvasFree ()
translate a1 a2 = liftF $ Translate a1 a2 ()
