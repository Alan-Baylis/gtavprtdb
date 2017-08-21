{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}

{- |
Module      : GTAVPRTDB.Binary
Description : Includes the methods for de- or -serialization with ByteString and the IO operations.
Copyright   : (C) Johann Lee <me@qinka.pro> 2017
License     : GPL-3
Maintainer  : me@qinka.pro qinka@live.com
Stability   : experimental
Portability : unknown

This module includes the serialization and deserialization with binary, and the IO operations include the reading and writing.
-}

-- | The binary format for data.

module GTAVPRTDB.Binary
  ( -- * Binary Seralizatino
    --
    -- $binary
    getImgs
  , putImgs
  , getLabels
  , putLabels
    -- ** Transform
  , toLabel
  , fromLabel
  , toImg
    -- ** render
  , renderFileI
  , renderFileL
    -- ** parse
  , parseFileI
  , parseFileL
    -- * io
    --
    -- $io
  , saveAnyImg
  , loadAnyImg
  , savePlateImg
  , loadPlateImg
  , saveCharImgs
  , loadTextureImg
  , loadFonts
  )where

import qualified Codec.Compression.Lzma as Lzma
import           Control.Monad
import qualified Data.Array.Repa        as R
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as BL
import           Data.List.Split
import qualified Data.Map               as M
import           Data.Monoid
import           Data.Proxy
import qualified Data.Vector            as V
import           GTAVPRTDB.Types

-- $binary
--
-- By using package binary, we can serialize and deserialize the images with the things.
-- The files of data, are compressed via lzma(xz), and the "integeral numbers" are storaged with big-endian.

-- | get for imags
getImgs :: Get [Img]
getImgs = do
  count <- fromIntegral <$> getWord64be
  pixels <- getLazyByteString $ 960 * 536 * 3 * count
  return $ V.fromList <$> chunksOf (960 * 536 * 3) (BL.unpack pixels)

-- | put for images
putImgs :: [Img] -> Put
putImgs imgs = do
  let len = length imgs
  putWord64be $ fromIntegral len
  let pixels = concat $ map V.toList imgs
  putLazyByteString $ BL.pack pixels

-- | get for labels
getLabels :: Get [Label]
getLabels = do
  count <- fromIntegral <$> getWord64be
  pixels <- replicateM ((4 * 2 + 1) * count) getInt16be
  return $ V.fromList <$> chunksOf (4 * 2 + 1) pixels

-- | put for labels
putLabels :: [Label] -> Put
putLabels labels = do
  let len = length labels
  putWord64be $ fromIntegral len
  let labels' = concat $ map V.toList labels
  mapM_ putInt16be labels'

-- | transform to traning data
toImg :: RecordImg 3 -> V.Vector Word8
toImg = V.fromList . R.toList . toRepa

-- | transform to label
toLabel :: [V2 Int32] -> Label
toLabel = let to (V2 a b) = fromIntegral <$> [a,b]
           in V.fromList . (1:) . concat . map to

-- | transform from label
fromLabel :: Label -> [Maybe (V2 Int32)]
fromLabel label =
  let from (a:b:_) = if status
                     then Just $ fromIntegral <$> V2 a b
                     else Nothing
      status = V.head label >  0
  in map from . chunksOf 2 . tail $ V.toList label

-- | render the traning data(file)
renderFileI :: FilePath -> [RecordImg 3] -> IO ()
renderFileI fp  = BL.writeFile fp . Lzma.compress .  runPut . putImgs . map toImg

-- | parse the traning data (file)
parseFileI :: FilePath -> IO [Img]
parseFileI fp = runGet getImgs . Lzma.decompress <$> BL.readFile fp

-- | render the traning data's label (file)
renderFileL :: FilePath -> [Label] -> IO ()
renderFileL fp = BL.writeFile fp . Lzma.compress . runPut . putLabels

-- | parse the traning data's label (file)
parseFileL :: FilePath -> IO [Label]
parseFileL fp = runGet getLabels . Lzma.decompress <$> BL.readFile fp


-- $io
--
-- io

-- | save any img
saveAnyImg :: OutputFormat -- ^ output format
           -> FilePath -- ^ file path
           -> Mat shape channels depth -- ^ image
           -> IO ()
saveAnyImg m fp img = B.writeFile fp $ exceptError $ imencode m img

-- | load any image
loadAnyImg :: (ToShapeDS (Proxy shape), ToChannelsDS (Proxy channels), ToDepthDS (Proxy depth))
           => ImreadMode -- ^ read mode
           -> FilePath -- ^ file path
           -> IO (Mat shape channels depth)
loadAnyImg m fp = exceptError . coerceMat . imdecode m <$> B.readFile fp

-- | save all fonts
saveCharImgs :: FilePath -> [(Char,CharImg)] -> IO ()
saveCharImgs fp mats = mapM_ (\(c,i) -> saveAnyImg OutputBmp (fp ++ ('/':c:".bmp")) i)  mats

-- | load texture
loadTextureImg :: ImreadMode -> FilePath -> IO TextureImg
loadTextureImg = loadAnyImg

-- | load the plate (for backgroud)
loadPlateImg :: FilePath
             -> IO (PlateImg 3)
loadPlateImg  = loadAnyImg ImreadUnchanged

-- | save plate (for mixed)
savePlateImg :: FilePath
             -> PlateImg 3
             -> IO ()
savePlateImg = saveAnyImg OutputBmp

-- | load font
loadFonts :: FilePath -- ^ the path for directory
          -> IO (M.Map Char CharImg)
loadFonts fp =
  let filenames = map (\c -> fp ++ ('/':c:".bmp")) fontList
  in M.fromList . zip fontList <$> mapM (loadAnyImg ImreadUnchanged) filenames
