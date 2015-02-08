{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.Static

image :: CanvasFree ()
image = do
  img <- newImage "arrows.png"
  drawImageSize img 0 0 300 100

main :: IO ()
main = writeCanvasDoc "Image.html" 625 325 image
