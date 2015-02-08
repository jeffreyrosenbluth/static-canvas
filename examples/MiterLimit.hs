{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.Static

miterlimit :: CanvasFree ()
miterlimit = do

    clearRect 0 0 150 150
    -- Draw guides
    strokeStyle (ColorStyle (Hex "#09f"))
    lineWidth    2
    strokeRect (-5) 50 160 50

    -- Set line styles
    strokeStyle (ColorStyle (Hex "#000"))
    lineWidth 10

    -- check input
    miterLimit 5

    -- Draw lines
    beginPath
    moveTo 0 100
    sequence_ [ lineTo ((fromIntegral i ** 1.5)*2) (75+(if i `mod` 2 == 0 then 25 else -25))
              | i <- [0..20] :: [Int]
              ]
    stroke

main :: IO ()
main = writeCanvasDoc "MiterLimit.html" 400 400 miterlimit
