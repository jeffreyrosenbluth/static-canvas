{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Static.Types
-- Copyright   :  (c) 2015 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jeffrey.rosenbluth@gmail.com
--
-- DSL for creating HTML5 Canvas.
--
-------------------------------------------------------------------------------

module Graphics.Static.Types where

import           Control.Applicative
import           Control.Monad.Free          (Free(..), liftF)
import           Control.Monad.Free.Church   (F, fromF)
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Double.Conversion.Text (toFixed)
import           Data.Text                   (pack)
import           Data.Text.Lazy.Builder

newtype Script a = Script {runScript :: (WriterT Builder (State Int) a)}
  deriving (Functor, Applicative, Monad, MonadWriter Builder, MonadState Int)

type CanvasFree = F Canvas

data Canvas r
  = Arc !Double !Double !Double !Double !Double !Bool r
  | ArcTo !Double !Double !Double !Double !Double r
  | BeginPath r
  | LineTo !Double !Double r
  | MoveTo !Double !Double r
  | Rect !Double !Double !Double !Double r
  | Stroke r
  | LinearGradient !Double !Double !Double !Double (Int -> r)
    deriving Functor


evalScript :: CanvasFree a -> Builder
evalScript c = (evalState . execWriterT . runScript . eval . fromF) c 0

record :: [Builder] -> Script ()
record = tell . mconcat

inc :: Script Int
inc = do
  n <- get
  put (n + 1)
  return n

eval :: Free Canvas a -> Script a
eval (Free (Arc a1 a2 a3 a4 a5 a6 c)) = do
  record ["ctx.arc("
         , jsDouble a1, comma, jsDouble a2, comma
         , jsDouble a3, comma, jsDouble a4, comma
         , jsDouble a5, comma, jsBool a6  , ");"]
  eval c
eval (Free (ArcTo a1 a2 a3 a4 a5 c)) = do
  record ["ctx.arcTo("
         , jsDouble a1, comma, jsDouble a2, comma
         , jsDouble a3, comma, jsDouble a4, comma
         , jsDouble a5, comma, ");"]
  eval c
eval (Free (BeginPath c)) = do
  tell "ctx.beginPath();"
  eval c
eval (Free (LineTo a1 a2 c)) = do
  record ["ctx.lineTo(", jsDouble a1, comma, jsDouble a2, ");"]
  eval c
eval (Free (MoveTo a1 a2 c)) = do
  record ["ctx.moveTo(", jsDouble a1, comma, jsDouble a2, ");"]
  eval c
eval (Free (Rect a1 a2 a3 a4 c)) = do
  record ["ctx.rect(", jsDouble a1, comma
                     , jsDouble a2, comma
                     , jsDouble a3, comma
                     , jsDouble a4, ");"]
  eval c
eval (Free (Stroke c)) = do
  tell "ctx.stroke();"
  eval c
eval (Free (LinearGradient a1 a2 a3 a4 k)) = do
  i <- inc
  record ["var gradient_", jsInt i, " = ctx.creatLinearGradient("
         , jsDouble a1, comma, jsDouble a2, comma
         , jsDouble a3, comma, jsDouble a4, ");"]
  eval (k i)
eval (Pure x) = return x

comma :: Builder
comma = singleton ','

jsBool :: Bool -> Builder
jsBool True = "true"
jsBool False = "false"

jsInt :: Int -> Builder
jsInt = fromText . pack . show

jsDouble :: Double -> Builder
jsDouble = fromText . toFixed 4

-------------------------------------------------------------------------------

arc :: Double -> Double -> Double -> Double -> Double -> Bool -> CanvasFree ()
arc a1 a2 a3 a4 a5 a6 = liftF $ Arc a1 a2 a3 a4 a5 a6 ()

arcTo :: Double -> Double -> Double -> Double -> Double -> CanvasFree ()
arcTo a1 a2 a3 a4 a5 = liftF $ ArcTo a1 a2 a3 a4 a5 ()

beginPath :: CanvasFree ()
beginPath = liftF $ BeginPath ()

moveTo :: Double -> Double -> CanvasFree ()
moveTo a1 a2 = liftF $ MoveTo a1 a2 ()

lineTo :: Double -> Double -> CanvasFree ()
lineTo a1 a2 = liftF $ LineTo a1 a2 ()

rect :: Double -> Double -> Double -> Double -> CanvasFree ()
rect a1 a2 a3 a4 = liftF $ Rect a1 a2 a3 a4 ()

stroke :: CanvasFree ()
stroke = liftF $ Stroke ()

linearGradient :: Double -> Double -> Double -> Double -> CanvasFree Int
linearGradient a1 a2 a3 a4 = liftF $ LinearGradient a1 a2 a3 a4 id
