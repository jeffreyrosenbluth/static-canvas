# static-canvas
A simple DSL for writing HTML5 Canvas in haskell and converting it
to Javascript. By static we mean non-interactive, so the parts of
the Canvas API that need to query the browser for run time information
like `isPointInPath(x, y)` are not included. This turns out to be
a surprisingly small part of HTML5 Canvas.

Here is Hello static-canvas with fancy text.

![Text](http://i.imgur.com/HGjSpJ6.png)

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
and here we draw a 10 pixel indigo colored line with round end caps.
There are plenty of other examples in [Examples](https://github.com/jeffreyrosenbluth/static-canvas/tree/master/examples).

![line](http://i.imgur.com/4Os9oxb.png)
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
