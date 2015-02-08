{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.Static
import Graphics.Static.ColorNames

linecolor :: CanvasFree ()
linecolor = do
  moveTo 100 150
  lineTo 450 50
  lineWidth 5
  strokeStyle goldenrod
  stroke

main :: IO ()
main = writeCanvasDoc "LineColor.html" 600 600 linecolor
