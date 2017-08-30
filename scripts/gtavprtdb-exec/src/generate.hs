{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

import           Control.Monad
import           Data.Maybe
import           GTAVPRTDB.Binary
import           GTAVPRTDB.CV
import           GTAVPRTDB.Database
import           GTAVPRTDB.Generate
import           GTAVPRTDB.Types
import           System.Console.CmdArgs
import           System.Directory
import           System.Environment
import           System.IO

data Generate = Generate { gFilePath :: FilePath
                         , gSize     :: Maybe Int
                         , gOut      :: FilePath
                         , gFormat   :: String
                         }
              deriving (Data,Show)

generate :: Generate
generate = Generate { gFilePath = "."
                      &= name "d"
                    , gSize = def
                      &= name "s"
                    , gOut = ".out"
                      &= name "o"
                    , gFormat = "binary"
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
  case gFormat of
    "figures" -> do
      let ms = zipWith drawKeyPoint (fromLabel <$> ls) rs
      createDirectoryIfMissing True gOut
      mapM_ (\(i,x) -> saveAnyImg OutputBmp (gOut ++ "/" ++ show i ++ ".bmp") x) $
        zip [0..] ms
    "binary" -> do
      renderFileI (gOut ++ ".tdz") rs
      renderFileL (gOut ++ ".tlz") ls
    'd':'b':'@':param -> do
      pipe <- connect (readHostPort param)
      zipWithM_ (writeData pipe master "master" "training") rs ls


