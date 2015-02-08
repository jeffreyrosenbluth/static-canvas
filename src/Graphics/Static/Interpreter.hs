{-# LANGUAGE OverloadedStrings          #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Static.Interpreter
-- Copyright   :  (c) 2015 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jeffrey.rosenbluth@gmail.com
--
-- DSL for creating HTML5 Canvas.
--
-------------------------------------------------------------------------------

module Graphics.Static.Interpreter
  ( evalScript
  ) where

import Control.Monad.Free         (Free(..))
import Control.Monad.Free.Church  (fromF)
import Control.Monad.State
import Control.Monad.Writer
import Data.Text.Lazy.Builder     (Builder, fromText, singleton)
import Graphics.Static.Javascript
import Graphics.Static.Types

evalScript :: CanvasFree a -> Builder
evalScript c = (evalState . execWriterT . runScript . eval . fromF) c 0

record :: [Builder] -> Script ()
record = tell . mconcat

inc :: Script Int
inc = do
  n <- get
  put (n + 1)
  return n

--------------------------------------------------------------------------------

eval :: Free Canvas a -> Script a

eval (Free (AddColorStop a1 a2 a3 c)) = do
  record [jsStyle a3, ".addColorStop("
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

eval (Free (CreateLinearGradient a1 a2 a3 a4 k)) = do
  i <- inc
  record ["var gradient_", jsInt i, " = ctx.createLinearGradient("
         , jsDouble a1, comma, jsDouble a2, comma
         , jsDouble a3, comma, jsDouble a4, ");"]
  eval (k (GradientStyle (LG i)))

eval (Free (CreateRadialGradient a1 a2 a3 a4 a5 a6 k)) = do
  i <- inc
  record ["var gradient_", jsInt i, " = ctx.createRadialGradient("
         , jsDouble a1, comma, jsDouble a2, comma
         , jsDouble a3, comma, jsDouble a4, comma
         , jsDouble a5, comma, jsDouble a6, ");"]
  eval (k (GradientStyle (RG i)))

eval (Free (DrawImageAt a1 a2 a3 c)) = do
  record ["image_", jsInt a1, ".onload = function() {ctx.drawImage(image_"
         , jsInt a1, comma, jsDouble a2, comma
         , jsDouble a3, ");};"]
  eval c

eval (Free (DrawImageSize a1 a2 a3 a4 a5 c)) = do
  record ["image_", jsInt a1, ".onload = function() {ctx.drawImage(image_"
         , jsInt a1, comma, jsDouble a2, comma
         , jsDouble a3 , comma, jsDouble a4, comma
         , jsDouble a5, ");};"]
  eval c

eval (Free (DrawImageCrop a1 a2 a3 a4 a5 a6 a7 a8 a9 c)) = do
  record ["image_", jsInt a1, ".onload = function() {ctx.drawImage(image_"
         , jsInt a1, comma, jsDouble a2, comma
         , jsDouble a3 , comma, jsDouble a4, comma
         , jsDouble a5, comma , jsDouble a6, comma
         , jsDouble a7, comma , jsDouble a8, comma
         , jsDouble a9, ");};"]
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
  record ["ctx.fillStyle = (", jsStyle a1, ");"]
  eval c

eval (Free (FillText a1 a2 a3 c)) = do
  record ["ctx.fillText('", fromText a1, singleton '\'', comma
         , jsDouble a2, comma
         , jsDouble a3, ");"]
  eval c

eval (Free (Font a1 c)) = do
  record ["ctx.font = ('", fromText a1, "');"]
  eval c

eval (Free (GlobalAlpha a1 c)) = do
  record ["ctx.globalAlpha = (", jsDouble a1, ");"]
  eval c
  
eval (Free (GlobalCompositeOperation a1 c)) = do
  record ["ctx.globalCompositeOperation = (", fromText a1, ");"]
  eval c

eval (Free (LineCap a1 c)) = do
  record ["ctx.lineCap = ('", jsLineCap a1, "');"]
  eval c

eval (Free (LineJoin a1 c)) = do
  record ["ctx.lineJoin = ('", jsLineJoin a1, "');"]
  eval c

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

eval (Free (NewImage a1 k)) = do
  i <- inc
  record ["var image_", jsInt i, " = new Image(); image_"
         , jsInt i, ".src = ('", fromText a1, "');"]
  eval (k i)
  
eval (Free (QuadraticCurveTo a1 a2 a3 a4 c)) = do
  record ["ctx.quadraticCurveTo("
         , jsDouble a1, comma, jsDouble a2, comma
         , jsDouble a3, comma, jsDouble a4, ");"]
  eval c

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
  record ["ctx.shadowColor = ('", jsColor a1, "');"]
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
  record ["ctx.strokeStyle = (", jsStyle a1, ");"]
  eval c

eval (Free (StrokeText a1 a2 a3 c)) = do
  record ["ctx.strokeText('", fromText a1, singleton '\''
         , comma, jsDouble a2, comma, jsDouble a3, ");"]
  eval c

eval (Free (TextAlign a1 c)) = do
  record ["ctx.textAlign = ('", jsTextAlign a1, "');"]
  eval c

eval (Free (TextBaseline a1 c)) = do
  record ["ctx.textBaseline = ('", jsTextBaseline a1, "');"]
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
