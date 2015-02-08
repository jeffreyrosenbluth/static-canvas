{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.Static

linecolor :: CanvasFree ()
linecolor = do
  moveTo 100 150
  lineTo 450 50
  lineWidth 5
  strokeStyle (ColorStyle (Hex "#ff0000"))
  stroke

main :: IO ()
main = writeCanvasDoc "LineColor.html" 600 600 linecolor
