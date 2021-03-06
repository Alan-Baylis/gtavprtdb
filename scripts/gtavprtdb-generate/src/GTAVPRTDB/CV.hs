{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}


{- |
Module      : GTAVPRTDB.CV
Description : Some unimportant about computer vision
Copyright   : (C) Johann Lee <me@qinka.pro> 2017
License     : GPL-3
Maintainer  : me@qinka.pro qinka@live.com
Stability   : experimental
Portability : unknown

Some important method for preprocess for training data.
-}

module GTAVPRTDB.CV
  ( resizeBg
  , drawKeyPoint
  ) where

import           Control.Monad.ST
import           Control.Monad.Trans
import           Data.Proxy
import           GTAVPRTDB.Types

-- | resize the image (background)
resizeBg :: Mat (S '[height, width]) channels depth
         -> RecordImg 3
resizeBg = exceptError . coerceMat . exceptError
  . resize (ResizeAbs $ toSize $ V2 960 536) InterCubic


-- | draw key points
drawKeyPoint :: Maybe (PointOffset Int32)
             -> RecordImg 3
             -> RecordImg 3
drawKeyPoint (Just off) plate = exceptError $ runST $ do
  p <- thaw plate
  eResult <- runExceptT $ mapM_
    (\x -> circle p x 10 (V4 0 0 1 0 :: V4 Double) (-1) LineType_AA 0) off
  case eResult of
    Left err -> pure $ throwE err
    Right () -> pure <$> unsafeFreeze p

drawKeyPoint Nothing plate = exceptError $ runST $ do
  p <- thaw plate
  eResult <- runExceptT $
    putText p
    "CAN NOT FOUND"
    (V2 0 0 :: V2 Int32)
    (Font FontHersheyDuplex NotSlanted 1.0)
    (V4 0 0 0 0 :: V4 Double)
    1
    LineType_AA
    False
  case eResult of
    Left err -> pure $ throwE err
    Right () -> pure <$> unsafeFreeze p
