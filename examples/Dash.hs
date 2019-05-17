{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RankNTypes        #-}

module Main where

import Graphics.Static

drawDashedLine :: ([Double], Double) -> MFC ()
drawDashedLine (ds, y) = do
  beginPath
  lineDash ds
  moveTo 0 y
  lineTo 300 y
  stroke

dash :: MFC ()
dash = mapM_ drawDashedLine $ zip ds ys
  where
    ds = [[], [1, 1], [10, 10], [20, 5], [15, 3, 3, 3]]
    ys = [15, 35..95]

main :: IO ()
main = writeCanvasDoc "Dash.html" 400 400 dash
