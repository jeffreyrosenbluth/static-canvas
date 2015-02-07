{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.Static
import Data.Text.Lazy.Builder

rectangle :: Builder
rectangle = evalScript $ do
  beginPath
  rect 188 50 200 100
  fillStyle (ColorStyle (Hex ("#FFD700")))
  fill
  lineWidth 7
  strokeStyle (ColorStyle (Hex ("#8B008B")))
  stroke

main :: IO ()
main = print $ toLazyText rectangle
