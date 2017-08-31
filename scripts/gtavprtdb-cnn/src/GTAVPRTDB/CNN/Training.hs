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
import           GTAVPRTDB.Types        (DataGetter, PointOffset (..))
import qualified TensorFlow.Core        as TF
import qualified TensorFlow.GenOps.Core as TF (less, sqrt, square, sum)
import qualified TensorFlow.Logging     as TF
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
            , TF.OneOf '[Float,Int32] (TLabel a)
            , TF.TensorDataType V.Vector (TLabel a)
            , TF.TensorDataType V.Vector (TInput a)
            , Show (TLabel a)
            )
         => TF.Build a -- ^ model
         -> Int -- ^ times
         -> Int -- ^ batch size
         -> DataGetter TF.Session (TInput a) (TLabel a) -- ^ training data getter
         -> DataGetter TF.Session (TInput a) (TLabel a) -- ^ testing data getter
         -> TF.Session (Float,TParam a)
training modelM times batchSize trainingGetter testingGetter = TF.withEventWriter ".log" $ \eventWriter -> do
  let al = 1
  liftIO $ putStrLn "Begin"
  summaryTensor <- TF.build TF.mergeAllSummaries
  model <- TF.build modelM
  TF.logGraph eventWriter modelM
  let times'                 = times `div` al
      (numH,numW,numC,numRT) = sizes model
      encodeInputBatch xs    =
        TF.encodeTensorData [genericLength xs, numH, numW, numC] $ mconcat xs
      encodeLabelBatch xs    =
        TF.encodeTensorData [genericLength xs,numRT] $ mconcat xs
  forM_ ([0..times] :: [Int]) $ \i -> do
    (inputs',labels') <- trainingGetter
    let inputs = encodeInputBatch inputs'
        labels = encodeLabelBatch labels'
    train model inputs labels
    when (i `mod` al == 0) $ do
      err <- errRt model inputs labels
      liftIO $ putStrLn $ show (i `div` al) ++ "/" ++ show times' ++ " training error: " ++ show err ++ "\n"
  (tinputs',tlabels') <- testingGetter
  let tinputs = encodeInputBatch tinputs'
      tlabels = encodeLabelBatch tlabels'
  errTest <- errRt model  tinputs tlabels
  liftIO $ putStrLn $ "training error(testing set): " ++ show (errTest * 100) ++ "%\n"
  infer model tinputs >>= (\x -> liftIO $ print x)
  p <- param model
  return (errTest,p)





{- |

@
img <- (\xs -> map (fmap fromIntegral) xs :: [V.Vector Float]) <$> parseFileI "out.tdz"
label <- (\xs -> map (fmap fromIntegral) xs :: [V.Vector Float]) <$> parseFileL "out.tlz"
rt <- training (createKPModel Nothing) 1000 img label img label
@
-}


-- | graph debug
{-
graphToTikz :: GraphDef
            ->
-}
