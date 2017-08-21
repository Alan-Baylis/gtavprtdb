{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}


{- |
Module      : GTAVPRTDB.CNN.Internal
Description : The example for using CNN to recognize vehicle's plate number.
Copyright   : (C) Johann Lee <me@qinka.pro> 2017
License     : GPL-3
Maintainer  : me@qinka.pro qinka@live.com
Stability   : experimental
Portability : unknown

The example for using CNN to recognize vehicle's plate number.
This module includes some basic method
-}

module GTAVPRTDB.CNN.Internal
  ( conv2D
  , maxPool
  ) where

import qualified Data.ByteString        as B
import           Data.Int
import           Data.Word
import           Len.Family2
import qualified TensorFlow.BuildOp     as TF (OpParams (..))
import qualified TensorFlow.GenOps.Core as TF (conv2D', maxPool')
import qualified TensorFlow.Output      as TF (OpDef (..), opAttr)
import qualified TensorFlow.Types       as TF


-- | conv 2d
conv2D :: TF.OneOf '[Word16,Double,Float] t
       => Tensor v'1   t -- ^ input
       -> Tensor v'2   t -- ^ filter
       -> Tensor Build t -- ^ output
conv2D = TF.conv2D' ( (TF.opAttr "strides"          .~ [1,1,1,1 :: Int64])
                    . (TF.opAttr "use_cudnn_on_gpu" .~ True)
                    . (TF.opAttr "padding"          .~ ("VALID" :: B.ByteString))
                    . (TF.opAttr "data_format"      .~ ("NHWC" :: B.ByteString))
                    )

-- | max pool
maxPool :: TF.OneOf '[Word16,Double,Float] t
        => (Int64,Int64) -- ^ ksize
        -> (Int64,Int64) -- ^ stride
        -> Tensor v'1   t -- ^ input
        -> Tensor Build t -- ^ output
maxPool (k1,k2) (s1,s2) = TF.maxPool' ( (TF.opAttr "ksize"       .~ [1,k1,k2,1])
                                      . (TF.opAttr "strides"     .~ [1,s1,s2,1])
                                      . (TF.opAttr "padding"     .~ ("VALID" :: B.ByteString))
                                      . (TF.opAttr "data_format" .~ ("NHWC" :: B.ByteString))
                                      )
