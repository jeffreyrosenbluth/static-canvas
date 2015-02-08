{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.Static

linecap :: CanvasFree ()
linecap = do
  sequence_
    [ do beginPath
         moveTo 200 (300 + n)
         lineTo 400 (300 + n)
         lineWidth 20
         strokeStyle (ColorStyle (Hex "#0000ff"))
         lineCap cap
         stroke
    | (cap , n) <- zip [LineCapButt, LineCapRound, LineCapSquare] [-50, 0 , 50]
    ]

main :: IO ()
main = writeCanvasDoc "LineCap.html" 600 600 linecap
