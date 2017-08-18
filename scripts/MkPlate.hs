-- | Create the plate
--
--   Firstly, the splited characters should be 28 x 56, the plate should be 256 x 128
--   Then, make up

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE PackageImports       #-}
{-# LANGUAGE TypeSynonymInstances #-}

module MkPlate where

import           "transformers" Control.Monad.Trans.Except
import qualified "bytestring" Data.ByteString            as B
import           Data.Char
import           Data.Int
import           Data.List                  (foldl')
import qualified "containers" Data.Map                   as M
import           Data.Monoid
import           Data.Proxy
import qualified "vector" Data.Vector                as V
import           Data.Word
import           Foreign.C.Types
import           "linear" Linear.V2                  (V2 (..))
import           "linear" Linear.V3                  (V3 (..))
import           "linear" Linear.V4                  (V4 (..))
import           "linear" Linear.Vector              (zero, (^+^))
import           "opencv" OpenCV

type CharImg     = Mat (ShapeT [56 ,28 ]) ('S 1) ('S Word8)
type PlateImg  a = Mat (ShapeT [128,256]) ('S a) ('S Word8)
type RecordImg a = Mat (ShapeT [536,960]) ('S a) ('S Word8)

fontList :: String
fontList = ['0'..'9'] ++ ['A'..'Z'] ++ ['_']

loadFonts :: FilePath -- ^ the path for directory
          -> IO (M.Map Char CharImg)
loadFonts fp = do
  let filenames = map (\c -> fp ++ ('/':c:".bmp")) fontList
  M.fromList . zip fontList <$> mapM loadImg filenames
  where loadImg :: FilePath -> IO CharImg
        loadImg fn =  exceptError . coerceMat . imdecode ImreadUnchanged <$> B.readFile fn

type RGBFunc = PlateImg 1 -> PlateImg 1 -> [PlateImg 1] -- | mask -> blank -> [channels]

blueMask :: RGBFunc
blueMask mask blank = [blank,mask,mask]
rgbMask :: Double -- ^ R
        -> Double -- ^ G
        -> Double -- ^ B
        -> RGBFunc
rgbMask r g b mask _ = map (\c -> mask `matScalarMult` c) [b,g,r]

mkMask :: M.Map Char CharImg -- ^ fonts
       -> RGBFunc -- ^ create RGb or rGB or RgB (lower case means blank)
       -> String -- ^ plate number
       -> PlateImg 3
mkMask maps rgbFunc plate' = exceptError . coerceMat . matMerge $ V.fromList $ rgbFunc maskr blkImg'
  where plate = let p = map toUpper $ take 8 plate'
                in if all (`elem` fontList) p then p else error ("unexcepted: " ++ plate')
        appendImg :: PlateImg 1 -> (Int32,CharImg) -> PlateImg 1
        appendImg blk (i,img) = exceptError $ matCopyTo blk (V2 (18 + 28*i) 49) img Nothing -- 20 -> y; 2+10*i -> x
        blkImg' :: PlateImg 1
        blkImg' = exceptError . coerceMat . exceptError $ mkMat [128,256 :: Int32] (1 :: Int32) Depth_8U (V4 0 0 0 0 :: V4 Double)
        blkImg  = cloneMat blkImg'
        maskr   :: PlateImg 1
        maskr   = foldl' appendImg blkImg $ zip [0..7] $ map (maps M.!) plate

maskPlate :: PlateImg 3 -- ^ plate
          -> PlateImg 3 -- ^ mask
          -> PlateImg 3 -- ^ rt
maskPlate = matSubtract

mkPlate ::  M.Map Char CharImg -- ^ fonts
        -> RGBFunc -- ^ create RGb or rGB or RgB (lower case means blank)
        -> String -- ^ plate number
        -> PlateImg 3 -- ^ plate
        -> PlateImg 3 -- ^ plate
mkPlate maps rgbFunc plateNumber plateImg = plateImg `maskPlate` mkMask maps rgbFunc plateNumber

loadPlateImg :: FilePath
             -> IO (PlateImg 3)
loadPlateImg fp = exceptError . coerceMat . imdecode ImreadUnchanged <$> B.readFile fp

savePlateImg :: FilePath
             -> PlateImg 3
             -> IO ()
savePlateImg fp img = B.writeFile fp $ exceptError $ imencode OutputBmp img

{- |

@
maps  <- loadFonts ".ignore"
platebg <- loadPlateImg ".ignore/plate01.jpg"
let plate =  mkPlate maps blueMask "07UMT674" platebg
savePlateImg ".ignore/test.bmp" plate
@

-}

type PointOffset a = [V2 a] -- (1,2) -> +2 x +1 -- tl,tr,bl,br

perspectiveTransformPlate :: PointOffset Float -- ^ offset
                          -> PlateImg  3
                          -> RecordImg 3
perspectiveTransformPlate off plate = tnsRecord
  where rawRecord :: RecordImg 3
        rawRecord = exceptError . coerceMat . exceptError $ mkMat [536,960 :: Int32] (3 :: Int32) Depth_8U (V4 0 0 0 0 :: V4 Double)
        rawWithPlate :: RecordImg 3
        rawWithPlate = exceptError $ matCopyTo rawRecord (V2 352 204) plate Nothing
        rawPoints = [V2 352 204,V2 608 204, V2 352 332, V2 608 332]
        offPoints = zipWith (+) rawPoints off
        toCFloat = (CFloat <$>)
        ptMat = getPerspectiveTransform (V.fromList $ toCFloat <$> rawPoints) (V.fromList $ toCFloat <$> offPoints)
        tnsRecord = exceptError $ warpPerspective rawWithPlate  ptMat InterCubic False True (BorderConstant $ toScalar (V4 0 0 0 0 :: V4 Double))

{- |

@
let ptPlate = perspectiveTransformPlate [V2 0 1, V2 0 (-1), V2 0 (-1), V2 0 1] plate
saveAnyImg OutputBmp ".ignore/pttest.bmp" ptPlate
@

-}


saveAnyImg :: OutputFormat
           -> FilePath
           -> Mat shape channels depth
           -> IO ()
saveAnyImg m fp img = B.writeFile fp $ exceptError $ imencode m img

loadAnyImg :: (ToShapeDS (Proxy shape), ToChannelsDS (Proxy channels), ToDepthDS (Proxy depth))
           => ImreadMode
           -> FilePath
           -> IO (Mat shape channels depth)
loadAnyImg m fp = exceptError . coerceMat . imdecode m <$> B.readFile fp


addPlateToBG :: RecordImg 3
             -> RecordImg 3
             -> RecordImg 3
addPlateToBG bg plate = exceptError $ matCopyTo bg (V2 0 0) plate (Just $ exceptError $ cvtColor bgr gray plate)

{- |

@
bg <- loadAnyImg ImreadUnchanged ".ignore/bg1.jpg"
let mixPlate = addPlateToBG bg ptPlate
saveAnyImg OutputBmp ".ignore/wbgtest.bmp" mixPlate
@

-}
