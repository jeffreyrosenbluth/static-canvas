{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Static.Internal
-- Copyright   :  (c) 2015 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jeffrey.rosenbluth@gmail.com
--
-- DSL for creating HTML5 Canvas.
--
-------------------------------------------------------------------------------

module Graphics.Static.Internal where

import Control.Applicative
import Control.Monad.Free          (Free(..))
import Control.Monad.Free.Church   (F, fromF)
import Control.Monad.State
import Control.Monad.Writer
import Data.Double.Conversion.Text (toFixed)
import Data.Text                   (Text, pack)
import Data.Text.Lazy.Builder      (Builder, fromText, singleton)

newtype Script a = Script {runScript :: (WriterT Builder (State Int) a)}
  deriving (Functor, Applicative, Monad, MonadWriter Builder, MonadState Int)

type CanvasFree = F Canvas

data Canvas r
  = AddColorStop !Double Color !Int r
  | Arc !Double !Double !Double !Double !Double !Bool r
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
  | FillStyle Style r
  | FillText Text !Double !Double r
  -- | Font
  | GlobalAlpha !Double r
  | GlobalCompositeOperation Text r
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
  | ShadowColor Color r
  | ShadowOffsetX !Double r
  | ShadowOffsetY !Double r
  | Stroke r
  | StrokeRect !Double !Double !Double !Double r
  | StrokeStyle Style r
  | StrokeText Text !Double !Double r
  -- | TextAlign
  -- | TextBaseLine
  | Transform !Double !Double !Double !Double !Double !Double r
  | Translate !Double !Double r
    deriving Functor

data Color = Hex  Text
           | RGB  !Int !Int !Int
           | RGBA !Int !Int !Int !Double

data Gradient = LG !Int
              | RG !Int

data Style = ColorStyle Color | GradientStyle Gradient


evalScript :: CanvasFree a -> Builder
evalScript c = (evalState . execWriterT . runScript . eval . fromF) c 0

record :: [Builder] -> Script ()
record = tell . mconcat

inc :: Script Int
inc = do
  n <- get
  put (n + 1)
  return n

comma :: Builder
comma = comma

jsBool :: Bool -> Builder
jsBool True = "true"
jsBool False = "false"

jsInt :: Int -> Builder
jsInt = fromText . pack . show

jsDouble :: Double -> Builder
jsDouble = fromText . toFixed 4

jsColor :: Color -> Builder
jsColor (Hex t) = fromText t
jsColor (RGB r g b)    = "rgb("  <> (fromText . pack $ show r)
                      <> comma   <> (fromText . pack $ show g)
                      <> comma   <> (fromText . pack $ show b) <> singleton ')'
jsColor (RGBA r g b a) = "rgba(" <> (fromText . pack $ show r)
                      <> comma   <> (fromText . pack $ show g)
                      <> comma   <> (fromText . pack $ show b)
                      <> comma   <> (jsDouble a) <> singleton ')'

jsStyle :: Style -> Builder
jsStyle (ColorStyle c) = jsColor c
jsStyle (GradientStyle (LG n)) = "gradient_" <> (fromText . pack . show $ n)
jsStyle (GradientStyle (RG n)) = "gradient_" <> (fromText . pack . show $ n)

--------------------------------------------------------------------------------

eval :: Free Canvas a -> Script a
eval (Free (AddColorStop a1 a2 a3 c)) = do
  record ["gradient_", jsInt a3, ".addColorStop("
         , jsDouble a1, comma, jsColor a2, ");"]
  eval c
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
eval (Free (FillStyle a1 c)) = do
  record ["ctx.fillStyle(", jsStyle a1, ");"]
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
eval (Free (ShadowColor a1 c)) = do
  record ["ctx.shadowColor = (", jsColor a1, ");"]
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
eval (Free (StrokeStyle a1 c)) = do
  record ["ctx.strokeStyle(", jsStyle a1, ");"]
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
