{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.Static
import Graphics.Static.ColorNames

shape :: CanvasFree ()
shape = do
  beginPath
  moveTo 170  80 
  bezierCurveTo 130  100  30  150  230  150 
  bezierCurveTo 250  180  320  180  340  150 
  bezierCurveTo 420  150  420  120  390  100 
  bezierCurveTo 430  40  370  30  340  50 
  bezierCurveTo 320  5  250  20  200  50 
  bezierCurveTo 200  5  100  20  170  80 

  closePath
  lineWidth 5
  fillStyle goldenrod
  fill
  strokeStyle blue
  stroke

main :: IO ()
main = writeCanvasDoc "Shape.html" 500 200 shape
