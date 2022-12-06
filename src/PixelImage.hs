module PixelImage where

import Codec.Picture as Pic
import Codec.Picture.Types

test filePath = do
  img <- Pic.readImage filePath
  case img of
    Left _       -> return ()
    Right image' -> Pic.savePngImage "./test-images/aboba.png" image'