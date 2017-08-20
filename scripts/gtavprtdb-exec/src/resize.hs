{-# LANGUAGE DataKinds #-}

import           GTAVPRTDB.Binary
import           GTAVPRTDB.CV
import           GTAVPRTDB.Types

main :: IO ()
main = do
  files <- lines <$> getContents
  flip mapM_ files $ \f -> do
    m <- loadAnyImg ImreadUnchanged f :: IO (Mat ('S ['D,'D]) D D)
    saveAnyImg (OutputJpeg defaultJpegParams) f $ resizeBg m

