{-# LANGUAGE DataKinds #-}

-- | The binary format for data.

module GTAVPRTDB.Binary where

import qualified Codec.Compression.Lzma as Lzma
import           Control.Monad
import qualified Data.Array.Repa        as R
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString.Lazy   as BL
import           Data.Int
import           Data.List.Split
import qualified Data.Vector            as V
import           Data.Word
import           GTAVPRTDB.MkPlate      (RecordImg (..))
import           Linear.V2              (V2 (..))
import           OpenCV

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

getImgs :: Get [Img]
getImgs = do
  count <- fromIntegral <$> getWord64be
  pixels <- getLazyByteString $ 960 * 536 * 3 * count
  return $ V.fromList <$> chunksOf (960 * 536 * 3) (BL.unpack pixels)

putImgs :: [Img] -> Put
putImgs imgs = do
  let len = length imgs
  putWord64be $ fromIntegral len
  let pixels = concat $ map V.toList imgs
  putLazyByteString $ BL.pack pixels

getLabels :: Get [Label]
getLabels = do
  count <- fromIntegral <$> getWord64be
  pixels <- replicateM ((4 * 2 + 1) * count) getInt16be
  return $ V.fromList <$> chunksOf (4 * 2 + 1) pixels

putLabels :: [Label] -> Put
putLabels labels = do
  let len = length labels
  putWord64be $ fromIntegral len
  let labels' = concat $ map V.toList labels
  mapM_ putInt16be labels'

toImg :: RecordImg 3 -> V.Vector Word8
toImg = V.fromList . R.toList . toRepa

toLabels :: [V2 Int32] -> Label
toLabels = let to (V2 a b) = fromIntegral <$> [a,b]
           in V.fromList . concat . map to


