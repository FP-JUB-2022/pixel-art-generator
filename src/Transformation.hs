module Transformation (
    transformFile,
    transformImage
) where

import Codec.Picture
import Data.List (genericLength)

data RGBAD = RGBAD Double Double Double Double

makeRGBA8FromRGBAD :: RGBAD -> PixelRGBA8
makeRGBA8FromRGBAD (RGBAD r g b a) = PixelRGBA8 (round r) (round g) (round b) (round a)

makeRGBADFromRGBA8 :: PixelRGBA8 -> RGBAD
makeRGBADFromRGBA8 (PixelRGBA8 r g b a) = RGBAD (fromIntegral r) (fromIntegral g) (fromIntegral b) (fromIntegral a)

getRGBADAvg :: [RGBAD] -> RGBAD
getRGBADAvg lst = RGBAD (r / len) (g / len) (b / len) (a / len) 
    where
        len = genericLength lst
        (RGBAD r g b a) = foldl1 (\(RGBAD r1 g1 b1 a1) (RGBAD r2 g2 b2 a2) -> RGBAD (r1 + r2) (g1 + g2) (b1 + b2) (a1 + a2)) lst

mergePixels :: [PixelRGBA8] -> PixelRGBA8
mergePixels pixels = makeRGBA8FromRGBAD (getRGBADAvg $ map makeRGBADFromRGBA8 pixels)

getLineIndices :: Int -> Int -> Int -> [Int]
getLineIndices imgSize kernelSize startPos = take kernelSize [startPos..(imgSize-1)]

getKernelIndices :: Image a -> Int -> Int -> (Int, Int) -> [(Int, Int)]
getKernelIndices img sx sy (sizeX, sizeY) = 
    let imgHeight = imageHeight img in
    let imgWidth = imageWidth img in
        [(x, y) | x <- getLineIndices imgWidth sizeX sx, y <- getLineIndices imgHeight sizeY sy]

computeResultingPixel :: Image PixelRGBA8 -> (Int, Int) -> Int -> Int -> PixelRGBA8
computeResultingPixel img (sizeX, sizeY) x y  | x `mod` sizeX /= 0 || y `mod` sizeY /= 0
    = computeResultingPixel img (sizeX, sizeY) (x - (x `mod` sizeX)) (y - (y `mod` sizeY))
computeResultingPixel img size sx sy
    = mergePixels $ map (uncurry (pixelAt img)) $ getKernelIndices img sx sy size


transformImage :: (Int, Int) -> Image PixelRGBA8 -> Image PixelRGBA8
transformImage kernelSize srcImg = 
    generateImage (computeResultingPixel srcImg kernelSize) (imageWidth srcImg) (imageHeight srcImg)


transformFile :: (Int, Int) -> FilePath -> FilePath -> IO()
transformFile kernelSize srcFile dstFile = do
    img <- readImage srcFile
    case img of
        Left _       -> return ()
        Right image' -> do
            savePngImage dstFile (ImageRGBA8 (transformImage kernelSize (convertRGBA8 image')))