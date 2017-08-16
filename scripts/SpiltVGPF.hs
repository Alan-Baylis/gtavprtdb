-- | This model is used to split the characters from the vehicle_generic_plate_font.jpg
--   You need use the OpenIV get the vehicle_generic_plate_Font.dds, and use OpenIV export it as .bmp.
--   Then you need to use GIMP or Photoshop to transform, or say translate, the texture.
--   The size of plate should be 256 x 128, and the size of the single character should be 28 x 56


{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE PackageImports       #-}
{-# LANGUAGE TypeSynonymInstances #-}

module SplitVGPF where

import           "transformers" Control.Monad.Trans.Except
import qualified "bytestring" Data.ByteString            as B
import           Data.Int
import           Data.Monoid
import           Data.Word
import           "linear" Linear.V2                  (V2 (..))
import           "linear" Linear.V3                  (V3 (..))
import           "linear" Linear.V4                  (V4 (..))
import           "linear" Linear.Vector              (zero, (^+^))
import           "opencv" OpenCV

type TextureImg = Mat (ShapeT [168,448]) ('S 1) ('S Word8)
type CharImg    = Mat (ShapeT [56 ,28 ]) ('S 1) ('S Word8)

-- | load the texture from file
loadImg :: ImreadMode -> FilePath -> IO TextureImg
loadImg readMode fp = exceptError . coerceMat . imdecode readMode <$> B.readFile fp


-- | filte one
splitFont :: TextureImg -> [(Char,CharImg)]
splitFont mat = map (getSingle mat) [0..36]
  where chars = ['0'..'9'] ++ ['A'..'Z'] ++ ['_']
        getSingle :: TextureImg -> Int32 -> (Char,CharImg)
        getSingle mat i' = let i = i' `mod` 37
                           in (chars !! (fromIntegral i) , exceptError . coerceMat . exceptError . matSubRect mat $ subRect i)
        subRect :: Int32 -> Rect2i
        subRect i = let x = ((i `mod` 16) * 28)
                        y = ((i `div` 16) * 56)
                    in toRect $ HRect (V2 x y) (V2 28 56)


saveCharImg :: FilePath -> [(Char,CharImg)] -> IO ()
saveCharImg fp mats = mapM_ saveSingle mats
  where saveSingle :: (Char,CharImg) -> IO ()
        saveSingle (c,mat) = B.writeFile (fp ++ ('/':c:".bmp")) $ exceptError $ imencode OutputBmp mat


{-|

Example:

@
mat <- loadImg ImreadUnchanged "vehicle_generic_plate_font.jpg"
saveCharImg "output" $ splitFont mat
@

-}
