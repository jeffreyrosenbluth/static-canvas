{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.Static

pattern :: CanvasFree ()
pattern = do
  img <- newImage "tile.png"
  onImageLoad img $ do
    ptn <- createPattern img Repeat
    rect 0 0 400 400
    fillStyle ptn
    fill

main :: IO ()
main = writeCanvasDoc "Pattern.html" 400 400 pattern
