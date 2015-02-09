{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.Static

image :: CanvasFree ()
image = do
  img <- newImage "arrows.png"
  onImageLoad img (drawImageAt img 0 0)

main :: IO ()
main = writeCanvasDoc "Image.html" 625 325 image
