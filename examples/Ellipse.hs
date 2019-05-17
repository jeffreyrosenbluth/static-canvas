{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Main where

import Graphics.Static

ellipse' :: MFC ()
ellipse' = do
  beginPath
  ellipse 100 100 50 75 (pi / 4) 0 (2 * pi) False
  fillStyle (ColorStyle (Hex ("#FFD700")))
  stroke
  fill
  lineWidth 7
  strokeStyle (ColorStyle (Hex ("#8B008B")))
  stroke

main :: IO ()
main = writeCanvasDoc "Ellipse.html" 400 400 ellipse'
