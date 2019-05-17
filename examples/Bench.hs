
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RankNTypes        #-}

module Main where

import Data.Foldable (for_)
import Graphics.Static

square :: MFC ()
square = do
  beginPath
  for_ [1..1000000] $
    \n -> do
      let m = n / 1000.0
      rect m m m m
      fillStyle (ColorStyle (Hex ("#FFD700")))
      strokeStyle (ColorStyle (Hex ("#8B008B")))
      fill
      stroke

main :: IO ()
main = writeCanvasDoc "Bench.html" 750 750 square
