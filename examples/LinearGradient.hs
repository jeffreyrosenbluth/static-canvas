{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Main where

import Graphics.Static

gradient :: MFC ()
gradient = do
  rect 0 0 200 400
  grd <- createLinearGradient 0 0 200 400
  addColorStop 0 (Hex "#8ed6ff") $ grd
  addColorStop 1 (Hex "#004cb3") $ grd
  fillStyle grd
  fill
  rect 0 0 400 400
  grd <- createLinearGradient 200 200 400 400
  addColorStop 0 (Hex "#BFC9CA") $ grd
  addColorStop 1 (Hex "#E67E22") $ grd
  fillStyle grd
  fill

main :: IO ()
main = writeCanvasDoc "LinearGradient.html" 400 400 gradient
