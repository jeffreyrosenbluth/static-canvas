{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.Static

gradient :: CanvasFree ()
gradient = do
  rect 0 0 400 400
  grd <- createLinearGradient 0 0 400 400
  addColorStop 0 (Hex "#8ed6ff") $ grd
  addColorStop 1 (Hex "#004cb3") $ grd
  fillStyle grd
  fill

main :: IO ()
main = writeCanvasDoc "LinearGradient.html" 400 400 gradient
