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
  (
  ) where

import           Data.Proxy
import           GTAVPRTDB.Types

-- | resize the image (background)
resizeBg :: Mat (S '[height, width]) channels depth
         -> RecordImg 3
resizeBg = exceptError . coerceMat . exceptError
  . resize (ResizeAbs $ toSize $ V2 960 536) InterCubic


