{-# LANGUAGE DataKinds #-}

{- |
Module      : GTAVPRTDB.Plate
Description : Generate the plate
Copyright   : (C) Johann Lee <me@qinka.pro> 2017
License     : GPL-3
Maintainer  : me@qinka.pro qinka@live.com
Stability   : experimental
Portability : unknown

Generate the license plate.
-}

module GTAVPRTDB.Plate
  ( -- * Basic op
    --
    -- $basic
    yellowMask
  , blueMask
  , mkPlate
    -- * transform
    --
    -- $transform
  , perspectiveTransformPlate
  , perspectiveTransformKeyPoints
    -- * combine
    --
    -- $combine
  , addPlateToBG
  ) where


import qualified Data.ByteString as B
import           Data.Char
import           Data.List
import qualified Data.Map        as M
import qualified Data.Vector     as V
import           Foreign.C.Types
import           GTAVPRTDB.Types

-- $basic
--
-- Create the basic plate
--
-- @
-- maps  <- loadFonts ".ignore"
-- platebg <- loadPlateImg ".ignore/plate01.jpg"
-- let plate =  mkPlate maps blueMask "07UMT674" platebg
-- savePlateImg ".ignore/test.bmp" plate
-- @

-- | blue character function
blueMask :: RGBFunc
blueMask mask blank plate = matSubtract plate $ exceptError $
  coerceMat $ matMerge $ V.fromList [blank,mask,mask]

-- | yellow character function
yellowMask :: RGBFunc
yellowMask mask blank plate = matAdd plate $ exceptError $
  coerceMat $ matMerge $ V.fromList
  [ blank
  , matScalarMult mask 0.65
  , matScalarMult mask 0.96
  ]

-- | create mask with string of plate
--
-- The char begin at (18,49) and in the horizontal direction.
-- The size of each character is 28 x 56
mkMask :: M.Map Char CharImg -- ^ fonts
       -> String -- ^ plate number
       -> (PlateImg 1,PlateImg 1)
mkMask maps plate' = (maskr,blkImg)
  where plate = let p = map toUpper $ take 8 plate'
                in if all (`elem` fontList) p
                   then p
                   else error ("unexcepted: " ++ plate')
        appendImg :: PlateImg 1 -> (Int32,CharImg) -> PlateImg 1
        appendImg blk (i,img) =
          -- 49 -> y; 18+28*i -> x
          exceptError $ matCopyTo blk (V2 (18 + 28*i) 49) img Nothing
        blkImg :: PlateImg 1
        blkImg = exceptError . coerceMat . exceptError $
          mkMat [128,256 :: Int32] (1 :: Int32) Depth_8U (V4 0 0 0 0 :: V4 Double)
        maskr   :: PlateImg 1
        maskr   = foldl' appendImg blkImg $ zip [0..7] $ map (maps M.!) plate

-- | mask plate to add characters to plate
maskPlate :: RGBFunc -- ^ create RGb or rGB or RgB (lower case means blank)
          -> PlateImg 3 -- ^ plate
          -> (PlateImg 1,PlateImg 1) -- ^ mask
          -> PlateImg 3 -- ^ rt
maskPlate func p (m,b) = func m b p

-- | create the plate with things
mkPlate ::  M.Map Char CharImg -- ^ fonts
        -> RGBFunc -- ^ create RGb or rGB or RgB (lower case means blank)
        -> String -- ^ plate number
        -> PlateImg 3 -- ^ plate
        -> PlateImg 3 -- ^ plate
mkPlate maps rgbFunc plateNumber plateImg =
  maskPlate rgbFunc plateImg $ mkMask maps plateNumber

-- $transform
--
-- Perspective transform plate to simulate vision looked at different angle.
-- The order of points are
--    * left top
--    * right top
--    * left bottom
--    * right bottom
--
--  The point (0,0) is at the left at the top of the image.
--
-- @
-- let ptPlate = perspectiveTransformPlate [V2 0 1, V2 0 (-1), V2 0 (-1), V2 0 1] plate
-- saveAnyImg OutputBmp ".ignore/pttest.bmp" ptPlate
-- @

-- | perspective transform for image
perspectiveTransformPlate :: PointOffset Float -- ^ offset
                          -> PlateImg  3
                          -> RecordImg 3
perspectiveTransformPlate off plate = tnsRecord
  where rawRecord :: RecordImg 3
        rawRecord = exceptError . coerceMat . exceptError $ mkMat
          [536,960 :: Int32] (3 :: Int32) Depth_8U (V4 0 0 0 0 :: V4 Double)
        rawWithPlate :: RecordImg 3
        rawWithPlate = exceptError $ matCopyTo rawRecord (V2 352 204) plate Nothing
        rawPoints = [V2 352 204,V2 608 204, V2 352 332, V2 608 332]
        offPoints = zipWith (+) rawPoints off
        toCFloat = (CFloat <$>)
        ptMat = getPerspectiveTransform (V.fromList $ toCFloat <$> rawPoints)
          (V.fromList $ toCFloat <$> offPoints)
        tnsRecord = exceptError $ warpPerspective rawWithPlate ptMat InterCubic
          False True (BorderConstant $ toScalar (V4 0 0 0 0 :: V4 Double))


-- | perspective transform for points
perspectiveTransformKeyPoints :: Num a => [V2 a] -> [V2 a]
perspectiveTransformKeyPoints off = zipWith (+) off
  [V2 352 204,V2 608 204, V2 352 332, V2 608 332]

-- $combine
--
-- add the plate characters to plate background
--
-- @
-- bg <- loadAnyImg ImreadUnchanged ".ignore/bg1.jpg"
-- let mixPlate = addPlateToBG bg ptPlate
-- saveAnyImg OutputBmp ".ignore/wbgtest.bmp" mixPlate
-- @

addPlateToBG :: RecordImg 3 -- ^ background
             -> RecordImg 3
             -> RecordImg 3
addPlateToBG bg plate = exceptError $ matCopyTo bg (V2 0 0) plate
  (Just $ exceptError $ cvtColor bgr gray plate)
