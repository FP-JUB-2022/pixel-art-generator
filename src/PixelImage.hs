module PixelImage (
  test,
  ) where

import Transformation (transformFile)

test :: IO ()
test = transformFile (100, 100) "./test-images/mario.png" "./test-images/out.png"