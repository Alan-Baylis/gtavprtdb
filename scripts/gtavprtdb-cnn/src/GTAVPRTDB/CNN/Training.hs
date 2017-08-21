{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE TypeFamilies          #-}

{- |
Module      : GTAVPRTDB.CNN.Training
Description : The example for using CNN to recognize vehicle's plate number.
Copyright   : (C) Johann Lee <me@qinka.pro> 2017
License     : GPL-3
Maintainer  : me@qinka.pro qinka@live.com
Stability   : experimental
Portability : unknown

The example for using CNN to recognize vehicle's plate number.
This module includes th method to help training
-}

module GTAVPRTDB.CNN.Training
  ( Training(..)
  , training
  ) where

import           Control.Monad          (forM_, when)
import           Control.Monad.IO.Class (liftIO)
import           Data.Int
import           Data.List
import           Data.Maybe
import qualified Data.Vector            as V
import           Data.Word
import           GTAVPRTDB.Binary       (fromLabel)
import           GTAVPRTDB.CNN.Internal
import           GTAVPRTDB.Types        (PointOffset (..))
import qualified TensorFlow.Core        as TF
import qualified TensorFlow.GenOps.Core as TF (less, sqrt, square, sum)
import qualified TensorFlow.Minimize    as TF
import qualified TensorFlow.Ops         as TF hiding (initializedVariable,
                                               zeroInitializedVariable)
import qualified TensorFlow.Variable    as TF


class Training a where
  type TInput a :: *
  type TLabel a :: *
  type TParam a :: *
  train :: a -- ^ item
        -> TF.TensorData (TInput a) -- ^ input
        -> TF.TensorData (TLabel a) -- ^ label
        -> TF.Session ()
  infer :: a -- ^ item
        -> TF.TensorData (TInput a) -- ^ input
        -> TF.Session (V.Vector [TLabel a]) -- ^ label
  errRt :: a -- ^ item
        -> TF.TensorData (TInput a) -- ^ input
        -> TF.TensorData (TLabel a) -- ^ label
        -> TF.Session Float -- ^ rate
  param :: a -- ^ item
        -> TF.Session (TParam a)
  sizes :: Integral i
        => a -- ^ item
        -> (i,i,i,i)

training :: ( Training a
            , TF.OneOf '[Word16,Double,Float] (TInput a)
            , TF.OneOf '[Int32] (TLabel a)
            , TF.TensorDataType V.Vector (TLabel a)
            , TF.TensorDataType V.Vector (TInput a)
            )
         => a -- model
         -> Int
         -> [[TInput a]] -- ^ training data
         -> [[TLabel a]] -- ^ training label
         -> [[TInput a]] -- ^ testing data
         -> [[TLabel a]] -- ^ testing label
         -> IO (Float,TParam a)
training model times trd trl ted tel = TF.runSession $ do
  let times'              = times `div`100
      (numW,numH,numC,numRT)    = sizes model
      encodeInputBatch xs =
        TF.encodeTensorData [genericLength xs, numW, numH, numC] $ V.fromList $ concat xs
      encodeLabelBatch xs =
        TF.encodeTensorData [genericLength xs,numRT] $ V.fromList $ concat xs
      batchSize           = 100
      selectInputBatch i xs = take batchSize $ drop (i * batchSize) (cycle xs)
      selectLabelBatch i xs = take batchSize $ drop (i * batchSize) (cycle xs)
  forM_ ([0..times] :: [Int]) $ \i -> do
    let inputs = encodeInputBatch (selectInputBatch i trd)
        labels = encodeLabelBatch (selectLabelBatch i trl)
    train model inputs labels
    when (i `mod` 100 == 0) $ do
      err <- errRt model inputs labels
      liftIO $ putStrLn $ show (i `div` 100) ++ "/" ++ show times ++ " training error: " ++ show (err * 100) ++ "%\n"
  let timages = encodeInputBatch ted
      tlabels = encodeLabelBatch tel
  errTest <- errRt model  timages tlabels
  liftIO $ putStrLn $ "training error(testing set): " ++ show (errTest * 100) ++ "%\n"
  p <- param model
  return (errTest,p)

