{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}

module GTAVPRTDB.Generate where

import           Control.Monad.Random
import           Data.Int
import           Data.List.Split
import qualified Data.Vector          as V
import           GTAVPRTDB.Binary
import           GTAVPRTDB.MkPlate
import           Linear.V2            (V2 (..))
import           OpenCV


generateSeed :: IO (String,Int,[Float])
generateSeed = (,,) <$> generateString <*> generatePlateI <*> generateLabel
  where generateString = take 8 <$> getRandoms >>=
          return . map (\i -> fontList !! mod i 37)
        generatePlateI = (`mod` 5) <$> getRandom
        generateOffsetA = nonLinearTrans1 90    <$> getRandomR (-90,90)
        generateOffsetB = nonLinearTrans1 180   <$> getRandomR (-180,180)
        generateScale   = nonLinearTrans2 0.4 2 <$> getRandomR (0.4,2)
        generateMoveX  = getRandomR (-480,480)
        generateMoveY  = getRandomR (-268,268)
        check a b s dx dy =
          let preIs = a <= 45 && a >= (-45)
                      && b <= 60 && b >= (-60)
                      && s <= 1.45 && s >= 0.65
              off = generateOffset a b s
              move = map (\(V2 x y) -> V2 (dx + x) (dy + y)) off
              is = and . map (\(V2 x y) -> x < 960 && x >= 0 && y < 536 && y >= 0) . perspectiveTransformKeyPoints
              move' = concat $ map (\(V2 x y) -> [x,y]) move
          in (if is move && preIs then 1 else 0) : move'
        generateLabel = check <$> generateOffsetA <*> generateOffsetB
         <*> generateScale  <*> generateMoveX <*> generateMoveY
        nonLinearTrans1 base a = a ^ 5 / base ^ 4
        nonLinearTrans2 b1 b2 a = nonLinearTrans1 (b2 - b1) a + b1



generateImg :: FilePath
            -> [FilePath]
            -> IO ([RecordImg 3],[V.Vector Int16])
generateImg fp bgFiles = do
  maps  <- loadFonts fp
  platebgs <- mapM (\i -> loadPlateImg $ fp ++ "plate0" ++ show i ++ ".jpg") [1..5]
  bgs <- mapM loadBg bgFiles
  unzip <$> mapM (create maps platebgs) bgs
  where create maps platebgs bg = do
          (str,i,l@(is:o)) <- generateSeed
          let mask    = if i `elem` [1,2] then yellowMask else blueMask
              plate   = mkPlate maps mask str (platebgs !! i)
              off = map (\(a:b:_) -> V2 a b) $ chunksOf 2 o
              ptPlate = perspectiveTransformPlate off plate
              record  = addPlateToBG bg ptPlate
              ls = concat $ map (\(V2 a b) -> round <$> [a,b]) $ perspectiveTransformKeyPoints off
          return (record,V.fromList (round is:ls))
        loadBg :: String -> IO (RecordImg 3)
        loadBg n = loadAnyImg ImreadUnchanged  $ fp ++ '/' : n


generateOffset :: Float -- ^ alpha |/  (for z axis) \in [-45,45]
               -> Float -- ^ beta  _/_ (for h(or x) axis \in [-60,60]
               -> Float -- ^ gamma \in [0.65,1.45]
               -> PointOffset Float
generateOffset a' b' g = [ V2 (-dH) (-dV)
                         , V2   dH  (-dV)
                         , V2 (-dH)   dV
                         , V2   dH    dV
                         ]
  where o al = 1 - sin al
        f al = 1 + sin al
        h = 256
        v = 168
        dLTX 
        dLTY
        dRTX
        dRTY
        dLBX
        dLBY
        dRBX
        dRBY
        dV = o a * sin b
        a'' = a' / 180 * pi
        b'' = b' / 180 * pi
        a = a'' * cos b''
        b = a'' * sin b''


