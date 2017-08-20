{-# LANGUAGE DataKinds #-}


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

import           Data.Proxy
import           GTAVPRTDB.Types
import Control.Monad.Trans

-- | resize the image (background)
resizeBg :: Mat (S '[height, width]) channels depth
         -> RecordImg 3
resizeBg = exceptError . coerceMat . exceptError
  . resize (ResizeAbs $ toSize $ V2 960 536) InterCubic


-- | draw key points
drawKeyPoint :: PointOffset Int32
             -> RecordImg 3
             -> RecordImg 3
drawKeyPoint off plate = exceptError $ withMatM
  (Proxy :: Proxy [536, 960])
  (Proxy :: Proxy 3)
  (Proxy :: Proxy Word8)
  (V4 0 0 0 0 :: V4 Double) $ \imgM -> do
  lift $ mapM_ (\x -> circle imgM x 10 (V4 0 0 1 0 :: V4 Double) (-1) LineType_AA 0) off
