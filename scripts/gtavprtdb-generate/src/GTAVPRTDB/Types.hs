{-# LANGUAGE DataKinds #-}

{- |
Module      : GTAVPRTDB.Types
Description : The basic types and re-exports for this package.
Copyright   : (C) Johann Lee <me@qinka.pro> 2017
License     : GPL-3
Maintainer  : me@qinka.pro qinka@live.com
Stability   : experimental
Portability : unknown

This module re-export some important modules, and define the important types.
-}

module GTAVPRTDB.Types
  ( -- * Types for image in generating
    --
    -- $generating
    TextureImg
  , CharImg
  , PlateImg
  , RecordImg
  , RGBFunc
  , PointOffset
    -- * Type for training.
    --
    -- $training
  , Img
  , Label
    -- * Constants
    --
    -- $const
  , fontList
    -- * others
    --
    -- TODO
  , DataGetter
    -- * re-export
    --
    -- $reexport
  , module Control.Monad.Trans.Except
  , module Data.Int
  , module Data.Word
  , module Linear.V2
  , module Linear.V3
  , module Linear.V4
  , module Linear.Vector
  , module OpenCV
  ) where


-- $reexport
--
-- The re-export export the commonly used modules in the other modules

import           Control.Monad.Trans.Except
import qualified Data.ByteString            as B
import           Data.Int
import qualified Data.Vector                as V
import           Data.Word
import           Linear.V2                  (V2 (..))
import           Linear.V3                  (V3 (..))
import           Linear.V4                  (V4 (..))
import           Linear.Vector              (zero, (^+^))
import           OpenCV

-- $generating

-- | The texture of characters is in size of 448 x 168.
--   In each line, there are 16 characters, and there are three lines, in total 36 characters.
--   And it is a single channel(gray) image.
type TextureImg = Mat (ShapeT [168,448]) ('S 1) ('S Word8)

-- | The size of character is 28 x 56.
--   It is a part of texture.
type CharImg    = Mat (ShapeT [56 ,28 ]) ('S 1) ('S Word8)

-- | The size of a plate is 256 x 128.
type PlateImg  a = Mat (ShapeT [128,256]) ('S a) ('S Word8)

-- | The size of a record, which means the generated img for training or background, or other images.
type RecordImg a = Mat (ShapeT [536,960]) ('S a) ('S Word8)

-- | mask -> blank -> plate -> plate
type RGBFunc = PlateImg 1 -> PlateImg 1 -> PlateImg 3 -> PlateImg 3

-- | the offset of point for transforms
type PointOffset a = [V2 a]


-- $training
--
-- These includes the images and labels


-- | Format for key points training datas
--   The size of a image is 960 x 536 x 3.
--   Images include a number which record the number of the data, and datas.
--   |-64-bit-unsigned-int-|-8-bit-unsigned-ints-|
--   BIGEndian
type Img   = V.Vector Word8

-- | Format for key points traning label
--   |-has-int16-|-lt-be-int16-x-|-lt-be-int16-y-|-rt-|-lb-|-rb-|
--   Left Top Right Bottom
--   BIGEndian
--   file struct like imgs
type Label = V.Vector Int16

-- * Constants
--
-- $const

-- | The list for characters.
--  {0..9,A..Z,_}
--  The '_' means the space.
fontList :: String
fontList = ['0'..'9'] ++ ['A'..'Z'] ++ ['_']



-- | get the data
type DataGetter m i l = m ([V.Vector i],[V.Vector l])
