{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.Static

linewidth :: CanvasFree ()
linewidth = do
  beginPath
  moveTo 100 150
  lineTo 450 50
  lineWidth 15
  stroke

main :: IO ()
main = writeCanvasDoc "LineWidth.html" 600 600 linewidth
