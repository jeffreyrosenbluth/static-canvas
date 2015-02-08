{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.Static

image :: CanvasFree ()
image = do
  img <- newImage "arrows.png"
  drawImageCrop img 0 0 600 300 150 150 200 200

main :: IO ()
main = writeCanvasDoc "Image.html" 625 325 image
