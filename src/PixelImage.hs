module PixelImage (
  test,
  ) where

import Transformation (transformFile)

test :: IO ()
test = transformFile (50, 50) "./test-images/mario.png" "./test-images/out.png"