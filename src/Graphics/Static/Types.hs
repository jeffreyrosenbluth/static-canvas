{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StrictData                 #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Static.Types
-- Copyright   :  (c) 2015 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jeffrey.rosenbluth@gmail.com
--
-- A small DSL for creating HTML5 Canvas.
--
-------------------------------------------------------------------------------

module Graphics.Static.Types where

import Control.Monad.Free.Class
import Control.Monad.State
import Control.Monad.Writer
import Data.Text                (Text)
import Data.Text.Lazy.Builder   (Builder)

newtype Script a = Script {runScript :: (WriterT Builder (State Int) a)}
  deriving (Functor, Applicative, Monad, MonadWriter Builder, MonadState Int)

-- | MonadFree Canvas
type MFC a = (forall m. MonadFree Canvas m => m a)

data Canvas r
  = AddColorStop Double Color Style r
  | Arc Double Double Double Double Double Bool r
  | ArcTo Double Double Double Double Double r
  | BeginPath r
  | BezierCurveTo Double Double Double Double Double Double r
  | ClearRect Double Double Double Double r
  | Clip r
  | ClosePath r
  | CreateLinearGradient Double Double Double Double (Style -> r)
  | CreatePattern Int RepeatStyle (Style -> r)
  | CreateRadialGradient Double Double Double Double Double Double (Style -> r)
  | DrawImageAt Int Double Double r
  | DrawImageSize Int Double Double Double Double r
  | DrawImageCrop Int Double Double Double Double Double Double Double Double r
  | Ellipse Double Double Double Double Double Double Double Bool r
  | Fill r
  | FillRect Double Double Double Double r
  | FillStyle Style r
  | FillText Text Double Double r
  | Font Text r
  | GlobalAlpha Double r
  | GlobalCompositeOperation CompositeOperation r
  | LineCap LineCapStyle r
  | LineDash [Double] r
  | LineJoin LineJoinStyle r
  | LineTo Double Double r
  | LineWidth Double r
  | MiterLimit Double r
  | MoveTo Double Double r
  | NewImage Text (Int -> r)
  | OnImageLoad Int (MFC ()) r
  | QuadraticCurveTo Double Double Double Double r
  | Rect Double Double Double Double r
  | Restore r
  | Rotate Double r
  | Save r
  | Scale Double Double r
  | SetTransform Double Double Double Double Double Double r
  | ShadowBlur Double r
  | ShadowColor Color r
  | ShadowOffsetX Double r
  | ShadowOffsetY Double r
  | Stroke r
  | StrokeRect Double Double Double Double r
  | StrokeStyle Style r
  | StrokeText Text Double Double r
  | TextAlign TextAlignStyle r
  | TextBaseline TextBaselineStyle r
  | Transform Double Double Double Double Double Double r
  | Translate Double Double r
    deriving Functor

data Color
  = Hex  Text
  | RGB  Int Int Int
  | RGBA Int Int Int Double

data Gradient
  = LG Int
  | RG Int

data Style
  = ColorStyle Color
  | GradientStyle Gradient
  | PatternStyle Int

data LineCapStyle
  = LineCapButt
  | LineCapRound
  | LineCapSquare

data LineJoinStyle
  = LineJoinMiter
  | LineJoinRound
  | LineJoinBevel

data TextAlignStyle
  = TextAlignStart
  | TextAlignEnd
  | TextAlignCenter
  | TextAlignLeft
  | TextAlignRight

data TextBaselineStyle
  = TextBaselineTop
  | TextBaselineHanging
  | TextBaselineMiddle
  | TextBaselineIdeographic
  | TextBaselineBottom

-- | For use with @createPattern@
data RepeatStyle
  = Repeat
  | RepeatX
  | RepeatY
  | NoRepeat

data CompositeOperation
  = SourceAtop
  | SourceIn
  | SourceOut
  | SourceOver
  | DestinationAtop
  | DestinationIn
  | DestinationOut
  | DestinationOver
  | Darker
  | Xor
  | Copy
