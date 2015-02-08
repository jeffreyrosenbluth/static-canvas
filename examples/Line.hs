{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.Static
import Graphics.Static.ColorNames

line :: CanvasFree ()
line = do
  beginPath
  moveTo 100 150
  lineTo 450 50
  lineWidth 10
  strokeStyle indigo
  lineCap LineCapRound
  stroke

main :: IO ()
main = writeCanvasDoc "Line.html" 500 200 line
