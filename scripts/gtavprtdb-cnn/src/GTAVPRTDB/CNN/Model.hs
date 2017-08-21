



{- |
Module      : GTAVPRTDB.CNN.Model
Description : The example for using CNN to recognize vehicle's plate number.
Copyright   : (C) Johann Lee <me@qinka.pro> 2017
License     : GPL-3
Maintainer  : me@qinka.pro qinka@live.com
Stability   : experimental
Portability : unknown

The example for using CNN to recognize vehicle's plate number.
This module includes the model for the CNN.
-}


module GTAVPRTDB.CNN.Model
  (
  ) where

import           GTAVPRTDB.CNN.Internal
import qualified TensorFlow.Core        as TF
import qualified TensorFlow.Minimize    as TF
import qualified TensorFlow.Ops         as TF hiding (initializedVariable,
                                               zeroInitializedVariable)
import qualified TensorFlow.Variable    as TF

{-
keyPointModel :: TF.MonadBuild m
              => Int64 -- ^ batch size
              ->
-}

-- | convolution layout
kpConvLayout :: TF.MonadBuild m
             -> (Int64,Int64) -- ^ ksize
             -> (Int64,Int64) -- ^ stride
             -> TF.Variable Float -- ^ weight, shape = [x,y,input_size,feature_size]
             -> TF.Variable Float -- ^ bias, shape = [feature_size]
             -> TF.Tensor t Float -- ^ input, shape = [w,h,f]
             -> m (TF.Tensor Build Float) -- ^ output
kpConvLayout k s w b i = pool
  where conv = TF.relu $ i `conv2D` w `TF.add` b
        pool = maxPool k s conv

-- | full connected layout
kpFCLayout :: TF.MonadBuild m
           => TF.Variable Float -- ^ weight, shape = [x,y]
           -> TF.Variable Float -- ^ bias, shape = [y]
           -> TF.Tensor t Float -- ^ input, shape = [w,h,f]
           -> m (TF.Tensor Build Float) -- ^ output
kpFCLayout w b i = i `TF.matMul` w `TF.add` b
