{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.Static
import Data.Text.Lazy.Builder

gradient :: Builder
gradient = evalScript $ do
  rect 0 0 400 400
  grd <- createLinearGradient 0 0 400 400
  addColorStop 0 (Hex "#8ed6ff") $ grd
  addColorStop 1 (Hex "#004cb3") $ grd
  fillStyle grd
  fill
  rect 0 0 200 200
  grd2 <- createLinearGradient 0 0 400 400
  addColorStop 0 (RGB 255 50 50) $ grd2
  addColorStop 1 (RGB 50 50 255) $ grd2
  fillStyle grd2
  fill

main :: IO ()
main = print $ toLazyText gradient
