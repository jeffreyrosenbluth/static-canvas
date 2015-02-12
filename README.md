# static-canvas [![Hackage](https://img.shields.io/hackage/v/static-canvas.svg?style=flat)](https://hackage.haskell.org/package/static-canvas)
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
There are plenty of examples in [Examples](https://github.com/jeffreyrosenbluth/static-canvas/tree/master/examples).
Here is one more showing how to use pattern to fill a rectangle.

![line](http://i.imgur.com/RRvyXyv.png)
```haskell
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
```
