

{- |
Module      : GTAVPRTDB.Texture
Description : Split the texture image into single charactors.
Copyright   : (C) Johann Lee <me@qinka.pro> 2017
License     : GPL-3
Maintainer  : me@qinka.pro qinka@live.com
Stability   : experimental
Portability : unknown

This module will split the texture image into single charactors.
-}

module GTAVPRTDB.Texture
  ( splitFonts
  ) where

import qualified Data.ByteString as B
import           GTAVPRTDB.Types


-- | split the fonts
splitFonts :: TextureImg       -- ^ texture image
           -> [(Char,CharImg)] -- ^ pair of images
splitFonts mat = map (getSingleFont mat) [0..36]

-- | get the sub-rectangle's location via index
subRect :: Int32 -- ^ index
        -> Rect2i
subRect i = let x = ((i `mod` 16) * 28)
                y = ((i `div` 16) * 56)
            in toRect $ HRect (V2 x y) (V2 28 56)

-- | Get the single font with index
getSingleFont :: TextureImg -- ^ texture image
              -> Int32 -- ^ index
              -> (Char,CharImg) -- ^ char and  image
getSingleFont mat i' =
  let i = i' `mod` 37
  in ( fontList !! (fromIntegral i)
     , exceptError . coerceMat . exceptError . matSubRect mat $ subRect i
     )



{-|

Example:

@
mat <- loadImg ImreadUnchanged "vehicle_generic_plate_font.jpg"
saveCharImg "output" $ splitFont mat
@

-}
