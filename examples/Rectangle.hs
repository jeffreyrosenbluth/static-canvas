{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.Static

rectangle :: CanvasFree ()
rectangle = do
  beginPath
  rect 188 50 200 100
  fillStyle (ColorStyle (Hex ("#FFD700")))
  fill
  lineWidth 7
  strokeStyle (ColorStyle (Hex ("#8B008B")))
  stroke

main :: IO ()
main = writeCanvasDoc "Rectangle.html" 400 400 rectangle
