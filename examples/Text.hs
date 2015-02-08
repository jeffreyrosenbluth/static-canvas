{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.Static
import Graphics.Static.ColorNames

text :: CanvasFree ()
text = do
  font "italic 60pt Calibri"
  lineWidth 6
  strokeStyle blue
  fillStyle goldenrod
  textBaseline TextBaselineMiddle
  strokeText "Hello" 150 100 
  fillText "Hello World!" 150 100

main :: IO ()
main = writeCanvasDoc "Text.html" 600 400 text
