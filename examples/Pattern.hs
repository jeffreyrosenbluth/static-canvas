{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.Static

pattern :: CanvasFree ()
pattern = do
  img <- newImage "arrows.png"
  onImageLoad img (do
    ptn <- createPattern img Repeat
    rect 0 0 1000 1000
    fillStyle ptn
    fill )

main :: IO ()
main = writeCanvasDoc "Pattern.html" 1000 1000 pattern
