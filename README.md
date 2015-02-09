# static-canvas
A tiny DSL for HTML5 Canvas
```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.Static
import Graphics.Static.ColorNames

line :: CanvasFree ()
line = do
  beginPath
  moveTo 100 150
  lineTo 450 50
  lineWidth 10
  strokeStyle indigo
  lineCap LineCapRound
  stroke

main :: IO ()
main = writeCanvasDoc "Line.html" 500 200 line
```
[[line!](http://i.imgur.com/4Os9oxb.png)]

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.Static
import Graphics.Static.ColorNames

text :: CanvasFree ()
text = do
  font "italic 60pt Calibri"
  lineWidth 6
  strokeStyle blue
  fillStyle goldenrod
  textBaseline TextBaselineMiddle
  strokeText "Hello" 150 100 
  fillText "Hello World!" 150 100

main :: IO ()
main = writeCanvasDoc "Text.html" 600 400 text
```
