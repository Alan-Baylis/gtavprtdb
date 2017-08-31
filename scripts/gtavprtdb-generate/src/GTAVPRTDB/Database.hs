{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : GTAVPRTDB.Database
Description : Includes the methods for de- or -serialization with ByteString and the IO operations.
Copyright   : (C) Johann Lee <me@qinka.pro> 2017
License     : GPL-3
Maintainer  : me@qinka.pro qinka@live.com
Stability   : experimental
Portability : unknown

This module includes the serialization and deserialization with binary, and the IO operations include the reading and writing.
-}

module GTAVPRTDB.Database
  ( openImageCursor
  , closeImageCursor
  , readBatchImageCursor
  , writeData
  , module Database.MongoDB
  ) where


import qualified Codec.Compression.Lzma as Lzma
import           Control.Monad.Trans
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString.Lazy   as B
import           Data.Maybe
import qualified Data.Vector            as V
import           Database.MongoDB       hiding (Label)
import           GTAVPRTDB.Binary       (toImg)
import           GTAVPRTDB.Types


-- | open reading cursor
openImageCursor :: MonadIO m
                => Pipe -- ^ pipe to connect with database
                -> AccessMode -- ^ access mode
                -> Database -- ^ database name
                -> Collection -- ^ collection name
                -> m Cursor
openImageCursor pipe accessMode database collection =
  access pipe accessMode database $
  find (select [] collection) { options = [NoCursorTimeout]}

-- | close reading cursor
closeImageCursor :: MonadIO m
                 => Pipe -- ^ pipe to connect with database
                 -> AccessMode -- ^ access mode
                 -> Database -- ^ database name
                 -> Cursor
                 -> m ()
closeImageCursor pipe accessMode database cursor =
  access pipe accessMode database $ closeCursor cursor


-- | read a batch of data with cursor
readBatchImageCursor :: MonadIO m
                     => Pipe -- ^ pipe to connect with database
                     -> AccessMode -- ^ access mode
                     -> Database -- ^ database name
                     -> Int -- ^ batch size
                     -> Cursor
                     -> m ([Img],[Label])
readBatchImageCursor pipe accessMode database size cursor =
  access pipe accessMode database $
   unzip . map docToData <$> nextN size cursor



-- | write the image to database
writeData :: MonadIO m
          => Pipe -- ^ pipe to connect with database
          -> AccessMode -- ^ access mode
          -> Database -- ^ database name
          -> Collection -- ^ collection name
          -> RecordImg 3
          -> Label
          -> m ()
writeData pipe accessMode database collection img label =
  access pipe accessMode database $ insert_ collection $
  dataToDoc (img,label)



docToData :: Document -> (Img,Label)
docToData doc =
  let uid        = fromMaybe (error $ show doc) $ doc !? "_id" :: UUID
      Binary bin = fromMaybe (error $ "cannot get image" ++ show uid) $
                   doc !? "image"
      image      = decode $ Lzma.decompress $ B.fromStrict bin
      label      = fromMaybe (error $ "cannot get label" ++ show uid) $
                   doc !? "label" :: [Int]
  in (V.fromList image,V.fromList $ map fromIntegral label)

dataToDoc :: (RecordImg 3,Label) -> Document
dataToDoc (img',label') =
  let img   = V.toList $ toImg img'
      bin   = Binary $ B.toStrict $ Lzma.compress $ encode img
      label = fromIntegral <$> V.toList label' :: [Int]
  in [ "label" =: label
     , "image" =: bin
     ]

