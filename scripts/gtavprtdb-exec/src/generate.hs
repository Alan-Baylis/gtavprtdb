{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}

import           Data.Maybe
import           GTAVPRTDB.Binary
import           GTAVPRTDB.CV
import           GTAVPRTDB.Generate
import           GTAVPRTDB.Types
import           System.Console.CmdArgs
import           System.Directory
import           System.Environment
import           System.IO

data Generate = Generate { gFilePath :: FilePath
                         , gSize     :: Maybe Int
                         , gOut      :: FilePath
                         , gFormat   :: Bool
                         }
              deriving (Data,Show)

generate :: Generate
generate = Generate { gFilePath = "."
                      &= name "d"
                    , gSize = def
                      &= name "s"
                    , gOut = ".out"
                      &= name "o"
                    , gFormat = False
                      &= name "f"
                    }

main :: IO ()
main = do
  Generate{..} <- cmdArgs generate
  files <- lines <$> getContents
  let size = case gSize of
        Just i -> take i
        _      -> id
  (rs,ls) <- generateImgs gFilePath $ size files
  if gFormat
    then do
    let ms = zipWith drawKeyPoint (fromLabel <$> ls) rs
    createDirectoryIfMissing True gOut
    mapM_ (\(i,x) -> saveAnyImg OutputBmp (gOut ++ "/" ++ show i ++ ".bmp") x) $
      zip [0..] ms
    else do
    renderFileI (gOut ++ ".tdz") rs
    renderFileL (gOut ++ ".tlz") ls


