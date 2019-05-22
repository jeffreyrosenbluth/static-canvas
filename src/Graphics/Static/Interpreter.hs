{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
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

import Control.Monad.Free.Church
import Control.Monad.State.Strict
import Data.Text                  (Text)
import Data.Text.Lazy.Builder     (Builder, fromText, singleton)
import Graphics.Static.Javascript
import Graphics.Static.Types

-- | Evaluate a static-canvas program and return the javascript code in a 'Builder'.
--   The first parameter should be a unique identifier to avoid name clashes with
--   other canvas elements in the html document.
evalScript :: Text -> MFC a -> Builder
evalScript t mfc = snd $ (execState . eval t) mfc (0, "")

record :: [Builder] -> Script ()
record bs = do
  (n, b) <- get
  put (n, mconcat (b:bs))

inc :: Script Int
inc = do
  (n, b) <- get
  put (n + 1, b)
  return n

--------------------------------------------------------------------------------

eval :: Text -> F Canvas a -> Script a
eval uniqId = iterM go
  where
    go :: Canvas (Script a) -> Script a
    go (AddColorStop a1 a2 a3 c) = do
      record [ jsStyle a3, ".addColorStop("
             , jsDouble a1, comma, jsColor a2, ");"]
      c

    go (Arc a1 a2 a3 a4 a5 a6 c) = do
      record [ fromText uniqId, "Ctx.arc("
             , jsDouble a1, comma, jsDouble a2, comma
             , jsDouble a3, comma, jsDouble a4, comma
             , jsDouble a5, comma, jsBool a6  , ");"]
      c

    go (ArcTo a1 a2 a3 a4 a5 c) = do
      record [ fromText uniqId, "Ctx.arcTo("
             , jsDouble a1, comma, jsDouble a2, comma
             , jsDouble a3, comma, jsDouble a4, comma
             , jsDouble a5, comma, ");"]
      c

    go (BeginPath c) = do
      record [fromText uniqId, "Ctx.beginPath();"]
      c

    go (BezierCurveTo a1 a2 a3 a4 a5 a6 c) = do
      record [ fromText uniqId, "Ctx.bezierCurveTo("
             , jsDouble a1, comma, jsDouble a2, comma
             , jsDouble a3, comma, jsDouble a4, comma
             , jsDouble a5, comma, jsDouble a6, ");"]
      c

    go (ClearRect a1 a2 a3 a4 c) = do
      record [ fromText uniqId, "Ctx.clearRect("
             , jsDouble a1, comma, jsDouble a2, comma
             , jsDouble a3, comma, jsDouble a4, ");"]
      c

    go (Clip c) = do
      record [fromText uniqId, "Ctx.clip();"]
      c

    go (ClosePath c) = do
      record [fromText uniqId, "Ctx.closePath();"]
      c

    go (CreateLinearGradient a1 a2 a3 a4 k) = do
      i <- inc
      record [ "var gradient_", jsInt i, " = ", fromText uniqId
             , "Ctx.createLinearGradient("
             , jsDouble a1, comma, jsDouble a2, comma
             , jsDouble a3, comma, jsDouble a4, ");"]
      k (GradientStyle (LG i))

    go (CreatePattern a1 a2 k) = do
      i <- inc
      record [ "var pattern_", jsInt i, " = ", fromText uniqId
             , "Ctx.createPattern(image_"
             , jsInt a1, comma, jsRepeat a2, ");"]
      k (PatternStyle i)

    go (CreateRadialGradient a1 a2 a3 a4 a5 a6 k) = do
      i <- inc
      record [ "var gradient_", jsInt i, " = ", fromText uniqId
             , "Ctx.createRadialGradient("
             , jsDouble a1, comma, jsDouble a2, comma
             , jsDouble a3, comma, jsDouble a4, comma
             , jsDouble a5, comma, jsDouble a6, ");"]
      k (GradientStyle (RG i))

    go (DrawImageAt a1 a2 a3 c) = do
      record [ fromText uniqId, "Ctx.drawImage(image_", jsInt a1, comma
             , jsDouble a2, comma, jsDouble a3, ");"]
      c

    go (DrawImageSize a1 a2 a3 a4 a5 c) = do
      record [ fromText uniqId, "Ctx.drawImage(image_", jsInt a1, comma
             , jsDouble a2, comma, jsDouble a3, comma
             , jsDouble a4, comma, jsDouble a5, ");"]
      c

    go (DrawImageCrop a1 a2 a3 a4 a5 a6 a7 a8 a9 c) = do
      record [ fromText uniqId, "Ctx.drawImage(image_", jsInt a1, comma
             , jsDouble a2, comma, jsDouble a3, comma
             , jsDouble a4, comma, jsDouble a5, comma
             , jsDouble a6, comma, jsDouble a7, comma
             , jsDouble a8, comma, jsDouble a9, ");"]
      c

    go (Ellipse a1 a2 a3 a4 a5 a6 a7 a8 c) = do
      record [ fromText uniqId, "Ctx.ellipse(", jsDouble a1, comma
             , jsDouble a2, comma, jsDouble a3, comma
             , jsDouble a4, comma, jsDouble a5, comma
             , jsDouble a6, comma, jsDouble a7, comma
             , jsBool a8, ");"]
      c

    go (Fill c) = do
      record [ fromText uniqId, "Ctx.fill();"]
      c

    go (FillRect a1 a2 a3 a4 c) = do
      record [ fromText uniqId, "Ctx.fillRect("
             , jsDouble a1, comma, jsDouble a2, comma
             , jsDouble a3, comma, jsDouble a4, ");"]
      c

    go (FillStyle a1 c) = do
      record [fromText uniqId, "Ctx.fillStyle = (", jsStyle a1, ");"]
      c

    go (FillText a1 a2 a3 c) = do
      record [ fromText uniqId, "Ctx.fillText('", fromText a1
             , singleton '\'', comma , jsDouble a2, comma
             , jsDouble a3, ");"]
      c

    go (Font a1 c) = do
      record [fromText uniqId, "Ctx.font = ('", fromText a1, "');"]
      c

    go (GlobalAlpha a1 c) = do
      record [fromText uniqId, "Ctx.globalAlpha = (", jsDouble a1, ");"]
      c

    go (GlobalCompositeOperation a1 c) = do
      record [ fromText uniqId, "Ctx.globalCompositeOperation = ('"
             , jsComposite a1, "');"]
      c

    go (LineCap a1 c) = do
      record [fromText uniqId, "Ctx.lineCap = ('", jsLineCap a1, "');"]
      c

    go (LineDash as c) = do
      record [fromText uniqId, "Ctx.setLineDash(", jsListDouble as, ");"]
      c

    go (LineJoin a1 c) = do
      record [fromText uniqId, "Ctx.lineJoin = ('", jsLineJoin a1, "');"]
      c

    go (LineTo a1 a2 c) = do
      record [ fromText uniqId, "Ctx.lineTo(", jsDouble a1, comma
             , jsDouble a2, ");"]
      c

    go (LineWidth a1 c) = do
      record [fromText uniqId, "Ctx.lineWidth = (", jsDouble a1, ");"]
      c

    go (MiterLimit a1 c) = do
      record [fromText uniqId, "Ctx.miterLimit = (", jsDouble a1, ");"]
      c

    go (MoveTo a1 a2 c) = do
      record [ fromText uniqId, "Ctx.moveTo(", jsDouble a1, comma
             , jsDouble a2, ");"]
      c

    go (NewImage a1 k) = do
      i <- inc
      record [ "var image_", jsInt i, " = new Image(); image_"
             , jsInt i, ".src = ('", fromText a1, "');"]
      k i

    go (OnImageLoad a1 a2 c) = do
      record [ "image_", jsInt a1, ".onload = function() {"
             , evalScript uniqId a2, "};"]
      c

    go (QuadraticCurveTo a1 a2 a3 a4 c) = do
      record [ fromText uniqId, "Ctx.quadraticCurveTo("
             , jsDouble a1, comma, jsDouble a2, comma
             , jsDouble a3, comma, jsDouble a4, ");"]
      c

    go (Rect a1 a2 a3 a4 c) = do
      record [ fromText uniqId, "Ctx.rect(", jsDouble a1, comma
             , jsDouble a2, comma
             , jsDouble a3, comma
             , jsDouble a4, ");"]
      c

    go (Restore c) = do
      record [fromText uniqId, "Ctx.restore();"]
      c

    go (Rotate a1 c) = do
      record [fromText uniqId, "Ctx.rotate(", jsDouble a1, ");"]
      c

    go (Save c) = do
      record [fromText uniqId, "Ctx.save();"]
      c

    go (Scale a1 a2 c) = do
      record [fromText uniqId, "Ctx.scale(", jsDouble a1, comma
             , jsDouble a2, ");"]
      c

    go (SetTransform a1 a2 a3 a4 a5 a6 c) = do
      record [ fromText uniqId, "Ctx.setTransform("
             , jsDouble a1, comma, jsDouble a2, comma
             , jsDouble a3, comma, jsDouble a4, comma
             , jsDouble a5, comma, jsDouble a6, ");"]
      c

    go (ShadowColor a1 c) = do
      record [fromText uniqId, "Ctx.shadowColor = ('", jsColor a1, "');"]
      c

    go (ShadowBlur a1 c) = do
      record [fromText uniqId, "Ctx.shadowBlur = (", jsDouble a1, ");"]
      c

    go (ShadowOffsetX a1 c) = do
      record [fromText uniqId, "Ctx.shadowOffsetX = (", jsDouble a1, ");"]
      c

    go (ShadowOffsetY a1 c) = do
      record [fromText uniqId, "Ctx.shadowOffsetY = (", jsDouble a1, ");"]
      c

    go (Stroke c) = do
      record [fromText uniqId, "Ctx.stroke();"]
      c

    go (StrokeRect a1 a2 a3 a4 c) = do
      record [ fromText uniqId, "Ctx.strokeRect("
             , jsDouble a1, comma, jsDouble a2, comma
             , jsDouble a3, comma, jsDouble a4, ");"]
      c

    go (StrokeStyle a1 c) = do
      record [fromText uniqId, "Ctx.strokeStyle = (", jsStyle a1, ");"]
      c

    go (StrokeText a1 a2 a3 c) = do
      record [ fromText uniqId, "Ctx.strokeText('", fromText a1
             , singleton '\'' , comma, jsDouble a2, comma
             , jsDouble a3, ");"]
      c

    go (TextAlign a1 c) = do
      record [fromText uniqId, "Ctx.textAlign = ('", jsTextAlign a1, "');"]
      c

    go (TextBaseline a1 c) = do
      record [ fromText uniqId, "Ctx.textBaseline = ('"
             , jsTextBaseline a1, "');"]
      c

    go (Transform a1 a2 a3 a4 a5 a6 c) = do
      record [ fromText uniqId, "Ctx.transform("
             , jsDouble a1, comma, jsDouble a2, comma
             , jsDouble a3, comma, jsDouble a4, comma
             , jsDouble a5, comma, jsDouble a6, ");"]
      c

    go (Translate a1 a2 c) = do
      record [ fromText uniqId, "Ctx.translate(", jsDouble a1, comma
             , jsDouble a2, ");"]
      c
    --
    -- go (Pure x) = return x
