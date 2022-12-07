module PixelImage (test) where

import Codec.Picture as Pic

test :: FilePath -> IO ()
test filePath = do
  img <- Pic.readImage filePath
  case img of
    Left _       -> return ()
    Right image' -> Pic.savePngImage "./test-images/aboba.png" image'