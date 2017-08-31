{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}


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
  ( KeyPointParameters(..)
  , KeyPointModel(..)
  , createKPModel
  , trainingData
  ) where

import           Control.Monad
import           Control.Monad.Trans
import           Data.Int
import           Data.IORef
import           Data.List.Split
import           Data.Maybe
import qualified Data.Vector            as V
import           GTAVPRTDB.Binary       (fromLabel)
import           GTAVPRTDB.CNN.Internal
import           GTAVPRTDB.CNN.Training
import           GTAVPRTDB.Database
import           GTAVPRTDB.Types        (DataGetter, PointOffset (..))
import qualified TensorFlow.Core        as TF
import qualified TensorFlow.GenOps.Core as TF (less, sqrt, square, sum)
import qualified TensorFlow.Minimize    as TF
import qualified TensorFlow.Ops         as TF hiding (initializedVariable,
                                               zeroInitializedVariable)
import qualified TensorFlow.Variable    as TF

-- $keypoints
--
--

-- | convolution layout
kpConvLayer :: (Int64,Int64) -- ^ ksize for maxpool
            -> (Int64,Int64) -- ^ stride for maxpool
            -> TF.Variable Float -- ^ weight, shape = [x,y,input_size,feature_size]
            -> TF.Variable Float -- ^ bias, shape = [feature_size]
            -> TF.Tensor t Float -- ^ input, shape = [w,h,f]
            -> TF.Tensor TF.Build Float -- ^ output
kpConvLayer k s w b i = pool
  where conv = TF.relu $ i `conv2D` TF.readValue w `TF.add`  TF.readValue b
        pool = maxPool k s conv

-- | full connected layout
kpFullLayer :: TF.Variable Float -- ^ weight, shape = [x,y]
            -> TF.Variable Float -- ^ bias, shape = [y]
            -> TF.Tensor t Float -- ^ input, shape = [w,h,f]
            -> TF.Tensor TF.Build Float -- ^ output
kpFullLayer w b i = TF.relu $ i `TF.matMul` TF.readValue w `TF.add`  TF.readValue b

data KeyPointParameters
  = KeyPointParameters { kpConv1 :: ([Float],[Float]) -- ^ weight and bias for the first layer: convolution layer
                       , kpConv2 :: ([Float],[Float]) -- ^ weight and bias for the second layer: convolution layer
                       , kpFull3 :: ([Float],[Float]) -- ^ weight and bias for the third layer: full connection
                       , kpFull4 :: ([Float],[Float]) -- ^ weight and bias for the last layer: full connection
                       }
    deriving Show


-- | create parameters
--
-- [error]
--  first layer:  [conv] 6 x 6 x  3 x 32 [maxpool] 6 x 4 (960 x 536 -> 960 x 536 -> 160 x 13)
-- second layer:  [conv] 5 x 5 x 32 x 64 [maxpool] 4 x 8 (160 x 135 -> 156 x 131 ->  39 x 19)
--  third layer:  [weight] 42240 -> 4096 [bias] 3333
--   last layer:  [weigjt] 3333 -> 9    [bias] 9
createKPParameters :: Maybe KeyPointParameters -- ^ parameters
                   -> TF.Build [TF.Variable Float] -- ^ w1 b1 w2 b2 ...
createKPParameters Nothing = do
  let value x y = TF.initializedVariable =<< randomParam x y
  -- convolution layers
  w1 <- value  6 [  6,  6,  3, 32]
  b1 <- TF.zeroInitializedVariable [32]
  w2 <- value  5 [  5,  5, 32, 64]
  b2 <- TF.zeroInitializedVariable [64]
  -- full connection layers
  w3 <- value 42432 [42432,3333]
  b3 <- TF.zeroInitializedVariable [3333]
  w4 <- value 3333 [3333,9]
  b4 <- TF.zeroInitializedVariable [9]
  return [w1,b1,w2,b2,w3,b3,w4,b4]
createKPParameters (Just KeyPointParameters{..}) = do
  let value x y = TF.initializedVariable $ TF.constant x y
  w1 <- value [  6,  6,  3, 32] $ fst kpConv1
  b1 <- value [             32] $ snd kpConv1
  w2 <- value [  5,  5, 32, 64] $ fst kpConv2
  b2 <- value [             64] $ snd kpConv2
  w3 <- value [    42432, 3333] $ fst kpFull3
  b3 <- value [           3333] $ snd kpFull3
  w4 <- value [        3333, 9] $ fst kpFull4
  b4 <- value [              9] $ snd kpFull4
  return [w1,b1,w2,b2,w3,b3,w4,b4]

-- | model of key point
data KeyPointModel
  = KeyPointModel { kpTrain :: TF.TensorData KPInput -- ^ images
                            -> TF.TensorData KPLabel -- ^ labels
                            -> TF.Session ()
                  , kpInfer :: TF.TensorData KPInput -- ^ images
                            -> TF.Session (V.Vector [KPLabel])
                  , kpErrRt :: TF.TensorData KPInput -- ^ images
                            -> TF.TensorData KPLabel -- ^ labels
                            -> TF.Session Float
                  , kpParam :: TF.Session KeyPointParameters
                  }

type KPInput = Float
type KPLabel = Float

instance Training KeyPointModel where
  type TInput KeyPointModel = KPInput
  type TLabel KeyPointModel = KPLabel
  type TParam KeyPointModel = KeyPointParameters
  train = kpTrain
  infer = kpInfer
  errRt = kpErrRt
  param = kpParam
  sizes _ = (536,960,3,9)

createKPModel :: Maybe KeyPointParameters -- ^ parameters
              -> TF.Build KeyPointModel   -- ^ model
createKPModel p = do
  images <- TF.placeholder [-1,536,960,3]
  w1:b1:w2:b2:w3:b3:w4:b4:_ <- createKPParameters p
  let out = kpFullLayer w4 b4 $                  -- /|\
            kpFullLayer w3 b3 $                  --  |
            reshape $                            --  |
            kpConvLayer (8,4) (8,4) w2 b2 $      --  |
            kpConvLayer (4,6) (4,6) w1 b1        --  |
            images                               --  |
      reshape x = TF.reshape x $ TF.constant [2] [-1,42432 :: Int32]
  labels <- TF.placeholder [-1,9] :: TF.Build (TF.Tensor TF.Value Float)
  let -- outI = TF.cast out :: TF.Tensor TF.Build Int32
      loss = TF.square $ labels `TF.sub` out
      params = w1:b1:w2:b2:w3:b3:w4:b4:[]
  trainStep <- TF.minimizeWith TF.adam loss params
  let diff            = TF.sum (TF.abs $ out `TF.sub` labels) (TF.scalar (1 :: Int32))
      diffAll         = TF.mean diff (TF.scalar (0 :: Int32))
      diffPredictions = diffAll `TF.less` TF.scalar 30
  -- errorRateTensor <- TF.render $ 1 - TF.reduceMean (TF.cast diffPredictions)
  return KeyPointModel
    { kpTrain = \imF lF ->
        TF.runWithFeeds_ [ TF.feed images imF
                         , TF.feed labels lF
                         ] trainStep
    , kpInfer = \imF -> do
       x <- V.toList <$> TF.runWithFeeds [TF.feed images imF] out
       return $ V.fromList $ chunksOf 9 x
    , kpErrRt = \imF lF ->
        TF.unScalar <$> TF.runWithFeeds [ TF.feed images imF
                                        , TF.feed labels lF
                                        ] diffAll
    , kpParam =
      let trans :: TF.Variable Float -> TF.Session [Float]
          trans x = V.toList <$> (TF.run =<< TF.render (TF.cast $ TF.readValue x))
      in KeyPointParameters
         <$> ((,) <$> trans w1 <*> trans b1)
         <*> ((,) <$> trans w2 <*> trans b2)
         <*> ((,) <$> trans w3 <*> trans b3)
         <*> ((,) <$> trans w4 <*> trans b4)
    }


trainingData :: Pipe -> Collection -> Int -> IORef Cursor ->  DataGetter TF.Session KPInput KPLabel
trainingData pipe collection batchSize cursorRef = do
  cursor        <- liftIO $ readIORef cursorRef
  (imgs,labels) <- readBatchImageCursor pipe master "master" batchSize cursor
  let len = length imgs
      renew = do
        closeImageCursor pipe master "master" cursor
        newCursor <- openImageCursor pipe master "master" collection
        liftIO $ writeIORef cursorRef newCursor
  when (len < batchSize) renew
  if len /= 0
    then do
    return ( map (fmap fromIntegral) imgs
           , map (fmap fromIntegral) labels
           )
    else trainingData pipe collection batchSize cursorRef

{- |

@
img <- (\xs -> map (fmap fromIntegral) xs :: [V.Vector Float]) <$> parseFileI "out.tdz"
label <- (\xs -> map (fmap fromIntegral) xs :: [V.Vector Float]) <$> parseFileL "out.tlz"
rt <- training (createKPModel Nothing) 1000 1 img label img label
@
-}
