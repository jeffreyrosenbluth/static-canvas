{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.Static

line :: CanvasFree ()
line = do
  beginPath
  moveTo 100 150
  lineTo 450 50
  stroke

main :: IO ()
main = writeCanvasDoc "Line.html" 600 600 line
