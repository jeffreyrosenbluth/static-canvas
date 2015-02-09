{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.Static
import Graphics.Static.ColorNames

text :: CanvasFree ()
text = do
  font "italic 57pt Calibri"
  lineWidth 6
  strokeStyle blue
  fillStyle goldenrod
  textBaseline TextBaselineMiddle
  strokeText "Hello" 25 100 
  fillText "Hello static-canvas!" 25 100

main :: IO ()
main = writeCanvasDoc "Text.html" 650 300 text
