{-# LANGUAGE OverloadedStrings #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Static.Interpreter
-- Copyright   :  (c) 2015 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jeffrey.rosenbluth@gmail.com
--
-- Interpreter to convert a static-canvas representation to js.
--
-------------------------------------------------------------------------------

module Graphics.Static.Interpreter
  ( evalScript
  ) where

import Control.Monad.Free         (Free(..))
import Control.Monad.Free.Church  (fromF)
import Control.Monad.State
import Control.Monad.Writer
import Data.Text                  (Text)
import Data.Text.Lazy.Builder     (Builder, fromText, singleton)
import Graphics.Static.Javascript
import Graphics.Static.Types

-- | Evaluate a static-canvas program and return the javascript code in a 'Builder'.
--   The first parameter should be a unique identifier to avoid name clashes with
--   other canvas elements in the html document.
evalScript :: Text -> CanvasFree a -> Builder
evalScript t c = (evalState . execWriterT . runScript . eval t . fromF) c 0

record :: [Builder] -> Script ()
record = tell . mconcat

inc :: Script Int
inc = do
  n <- get
  put (n + 1)
  return n

--------------------------------------------------------------------------------

eval :: Text -> Free Canvas a -> Script a

eval uniqId (Free (AddColorStop a1 a2 a3 c)) = do
  record [jsStyle a3, ".addColorStop("
         , jsDouble a1, comma, jsColor a2, ");"]
  eval uniqId c

eval uniqId (Free (Arc a1 a2 a3 a4 a5 a6 c)) = do
  record [fromText uniqId, "Ctx.arc("
         , jsDouble a1, comma, jsDouble a2, comma
         , jsDouble a3, comma, jsDouble a4, comma
         , jsDouble a5, comma, jsBool a6  , ");"]
  eval uniqId c

eval uniqId (Free (ArcTo a1 a2 a3 a4 a5 c)) = do
  record [fromText uniqId, "Ctx.arcTo("
         , jsDouble a1, comma, jsDouble a2, comma
         , jsDouble a3, comma, jsDouble a4, comma
         , jsDouble a5, comma, ");"]
  eval uniqId c

eval uniqId (Free (BeginPath c)) = do
  record [fromText uniqId, "Ctx.beginPath();"]
  eval uniqId c

eval uniqId (Free (BezierCurveTo a1 a2 a3 a4 a5 a6 c)) = do
  record [fromText uniqId, "Ctx.bezierCurveTo("
         , jsDouble a1, comma, jsDouble a2, comma
         , jsDouble a3, comma, jsDouble a4, comma
         , jsDouble a5, comma, jsDouble a6, ");"]
  eval uniqId c

eval uniqId (Free (ClearRect a1 a2 a3 a4 c)) = do
  record [fromText uniqId, "Ctx.clearRect("
         , jsDouble a1, comma, jsDouble a2, comma
         , jsDouble a3, comma, jsDouble a4, ");"]
  eval uniqId c

eval uniqId (Free (Clip c)) = do
 record [fromText uniqId, "Ctx.clip();"]
 eval uniqId c

eval uniqId (Free (ClosePath c)) = do
  record [fromText uniqId, "Ctx.closePath();"]
  eval uniqId c

eval uniqId (Free (CreateLinearGradient a1 a2 a3 a4 k)) = do
  i <- inc
  record ["var gradient_", jsInt i, " = ", fromText uniqId, "Ctx.createLinearGradient("
         , jsDouble a1, comma, jsDouble a2, comma
         , jsDouble a3, comma, jsDouble a4, ");"]
  eval uniqId (k (GradientStyle (LG i)))

eval uniqId (Free (CreatePattern a1 a2 k)) = do
  i <- inc
  record ["var pattern_", jsInt i, " = ", fromText uniqId, "Ctx.createPattern(image_"
         , jsInt a1, comma, jsRepeat a2, ");"]
  eval uniqId (k (PatternStyle i))

eval uniqId (Free (CreateRadialGradient a1 a2 a3 a4 a5 a6 k)) = do
  i <- inc
  record ["var gradient_", jsInt i, " = ", fromText uniqId, "Ctx.createRadialGradient("
         , jsDouble a1, comma, jsDouble a2, comma
         , jsDouble a3, comma, jsDouble a4, comma
         , jsDouble a5, comma, jsDouble a6, ");"]
  eval uniqId (k (GradientStyle (RG i)))

eval uniqId (Free (DrawImageAt a1 a2 a3 c)) = do
  record [fromText uniqId, "Ctx.drawImage(image_", jsInt a1, comma
         , jsDouble a2, comma, jsDouble a3, ");"]
  eval uniqId c

eval uniqId (Free (DrawImageSize a1 a2 a3 a4 a5 c)) = do
  record [fromText uniqId, "Ctx.drawImage(image_", jsInt a1, comma
         , jsDouble a2, comma, jsDouble a3, comma
         , jsDouble a4, comma, jsDouble a5, ");"]
  eval uniqId c

eval uniqId (Free (DrawImageCrop a1 a2 a3 a4 a5 a6 a7 a8 a9 c)) = do
  record [fromText uniqId, "Ctx.drawImage(image_", jsInt a1, comma
         , jsDouble a2, comma, jsDouble a3, comma
         , jsDouble a4, comma, jsDouble a5, comma
         , jsDouble a6, comma, jsDouble a7, comma
         , jsDouble a8, comma, jsDouble a9, ");"]
  eval uniqId c

eval uniqId (Free (Fill c)) = do
  record [fromText uniqId, "Ctx.fill();"]
  eval uniqId c

eval uniqId (Free (FillRect a1 a2 a3 a4 c)) = do
  record [fromText uniqId, "Ctx.fillRect("
         , jsDouble a1, comma, jsDouble a2, comma
         , jsDouble a3, comma, jsDouble a4, ");"]
  eval uniqId c

eval uniqId (Free (FillStyle a1 c)) = do
  record [fromText uniqId, "Ctx.fillStyle = (", jsStyle a1, ");"]
  eval uniqId c

eval uniqId (Free (FillText a1 a2 a3 c)) = do
  record [fromText uniqId, "Ctx.fillText('", fromText a1, singleton '\'', comma
         , jsDouble a2, comma
         , jsDouble a3, ");"]
  eval uniqId c

eval uniqId (Free (Font a1 c)) = do
  record [fromText uniqId, "Ctx.font = ('", fromText a1, "');"]
  eval uniqId c

eval uniqId (Free (GlobalAlpha a1 c)) = do
  record [fromText uniqId, "Ctx.globalAlpha = (", jsDouble a1, ");"]
  eval uniqId c

eval uniqId (Free (GlobalCompositeOperation a1 c)) = do
  record [fromText uniqId, "Ctx.globalCompositeOperation = ('", jsComposite a1, "');"]
  eval uniqId c

eval uniqId (Free (LineCap a1 c)) = do
  record [fromText uniqId, "Ctx.lineCap = ('", jsLineCap a1, "');"]
  eval uniqId c

eval uniqId (Free (LineJoin a1 c)) = do
  record [fromText uniqId, "Ctx.lineJoin = ('", jsLineJoin a1, "');"]
  eval uniqId c

eval uniqId (Free (LineTo a1 a2 c)) = do
  record [fromText uniqId, "Ctx.lineTo(", jsDouble a1, comma, jsDouble a2, ");"]
  eval uniqId c

eval uniqId (Free (LineWidth a1 c)) = do
  record [fromText uniqId, "Ctx.lineWidth = (", jsDouble a1, ");"]
  eval uniqId c

eval uniqId (Free (MiterLimit a1 c)) = do
  record [fromText uniqId, "Ctx.miterLimit = (", jsDouble a1, ");"]
  eval uniqId c

eval uniqId (Free (MoveTo a1 a2 c)) = do
  record [fromText uniqId, "Ctx.moveTo(", jsDouble a1, comma, jsDouble a2, ");"]
  eval uniqId c

eval uniqId (Free (NewImage a1 k)) = do
  i <- inc
  record ["var image_", jsInt i, " = new Image(); image_"
         , jsInt i, ".src = ('", fromText a1, "');"]
  eval uniqId (k i)

eval uniqId (Free (OnImageLoad a1 a2 c)) = do
  record ["image_", jsInt a1, ".onload = function() {", evalScript uniqId a2, "};"]
  eval uniqId c

eval uniqId (Free (QuadraticCurveTo a1 a2 a3 a4 c)) = do
  record [fromText uniqId, "Ctx.quadraticCurveTo("
         , jsDouble a1, comma, jsDouble a2, comma
         , jsDouble a3, comma, jsDouble a4, ");"]
  eval uniqId c

eval uniqId (Free (Rect a1 a2 a3 a4 c)) = do
  record [fromText uniqId, "Ctx.rect(", jsDouble a1, comma
                     , jsDouble a2, comma
                     , jsDouble a3, comma
                     , jsDouble a4, ");"]
  eval uniqId c

eval uniqId (Free (Restore c)) = do
  record [fromText uniqId, "Ctx.restore();"]
  eval uniqId c

eval uniqId (Free (Rotate a1 c)) = do
  record [fromText uniqId, "Ctx.rotate(", jsDouble a1, ");"]
  eval uniqId c

eval uniqId (Free (Save c)) = do
  record [fromText uniqId, "Ctx.save();"]
  eval uniqId c

eval uniqId (Free (Scale a1 a2 c)) = do
  record [fromText uniqId, "Ctx.scale(", jsDouble a1, comma, jsDouble a2, ");"]
  eval uniqId c

eval uniqId (Free (SetTransform a1 a2 a3 a4 a5 a6 c)) = do
  record [fromText uniqId, "Ctx.setTransform("
         , jsDouble a1, comma, jsDouble a2, comma
         , jsDouble a3, comma, jsDouble a4, comma
         , jsDouble a5, comma, jsDouble a6, ");"]
  eval uniqId c

eval uniqId (Free (ShadowColor a1 c)) = do
  record [fromText uniqId, "Ctx.shadowColor = ('", jsColor a1, "');"]
  eval uniqId c

eval uniqId (Free (ShadowBlur a1 c)) = do
  record [fromText uniqId, "Ctx.shadowBlur = (", jsDouble a1, ");"]
  eval uniqId c

eval uniqId (Free (ShadowOffsetX a1 c)) = do
  record [fromText uniqId, "Ctx.shadowOffsetX = (", jsDouble a1, ");"]
  eval uniqId c

eval uniqId (Free (ShadowOffsetY a1 c)) = do
  record [fromText uniqId, "Ctx.shadowOffsetY = (", jsDouble a1, ");"]
  eval uniqId c

eval uniqId (Free (Stroke c)) = do
  record [fromText uniqId, "Ctx.stroke();"]
  eval uniqId c

eval uniqId (Free (StrokeRect a1 a2 a3 a4 c)) = do
  record [fromText uniqId, "Ctx.strokeRect("
         , jsDouble a1, comma, jsDouble a2, comma
         , jsDouble a3, comma, jsDouble a4, ");"]
  eval uniqId c

eval uniqId (Free (StrokeStyle a1 c)) = do
  record [fromText uniqId, "Ctx.strokeStyle = (", jsStyle a1, ");"]
  eval uniqId c

eval uniqId (Free (StrokeText a1 a2 a3 c)) = do
  record [fromText uniqId, "Ctx.strokeText('", fromText a1, singleton '\''
         , comma, jsDouble a2, comma, jsDouble a3, ");"]
  eval uniqId c

eval uniqId (Free (TextAlign a1 c)) = do
  record [fromText uniqId, "Ctx.textAlign = ('", jsTextAlign a1, "');"]
  eval uniqId c

eval uniqId (Free (TextBaseline a1 c)) = do
  record [fromText uniqId, "Ctx.textBaseline = ('", jsTextBaseline a1, "');"]
  eval uniqId c

eval uniqId (Free (Transform a1 a2 a3 a4 a5 a6 c)) = do
  record [fromText uniqId, "Ctx.transform("
         , jsDouble a1, comma, jsDouble a2, comma
         , jsDouble a3, comma, jsDouble a4, comma
         , jsDouble a5, comma, jsDouble a6, ");"]
  eval uniqId c

eval uniqId (Free (Translate a1 a2 c)) = do
  record [fromText uniqId, "Ctx.translate(", jsDouble a1, comma, jsDouble a2, ");"]
  eval uniqId c

eval _ (Pure x) = return x
