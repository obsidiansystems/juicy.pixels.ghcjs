-- test file, don't care about unused, on the contrary...
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
import System.Environment

import Data.Binary
import Data.Bits ( unsafeShiftR, xor )
import Data.Char( toLower )
import Data.List( isInfixOf, isPrefixOf )
import Data.Monoid
import Data.Word( Word8 )
import Control.Monad( forM_, liftM, when )
import System.FilePath
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import Codec.Picture.Types
import Codec.Picture.Bitmap( encodeBitmap, encodeBitmapWithMetadata )
import qualified Codec.Picture.Metadata as Met
import qualified Codec.Picture.Metadata.Exif as Met
import qualified Data.Vector.Storable as V

import Control.Applicative( (<$>) )
import Criterion.Main
import Control.DeepSeq


import Util

greyScaleWitness :: Int -> Image Pixel8
greyScaleWitness offset = img 232 241
    where img w h = Image w h $ V.fromListN (w * h) $ pixels w h
          pixels w h = [pixel (x + offset) (y + offset)
                                | y <- [0 .. h-1], x <- [0 .. w-1] ]
          pixel x y = truncate $ sqrt dist
                where xf = fromIntegral $ x - 100 :: Int
                      yf = fromIntegral $ y - 100
                      dist = (fromIntegral $ xf * xf + yf * yf) :: Double

bmpValidTests :: [FilePath]
bmpValidTests =
    ["simple_bitmap_24bits.bmp"
    ,"simple_bitmap_8bits.bmp"
    ,"simple_bitmap_32bits.bmp"
    ,"bitmap_32bits_type0.bmp"
    ,"eggyra0001.bmp"
    ,"smiley.bmp"
    ,"rle_test.bmp"
    ,"bmpsuite_good/pal1bg.bmp"
    ,"bmpsuite_good/pal8.bmp"
    ,"bmpsuite_good/pal8v5.bmp"
    ,"bmpsuite_good/rgb16.bmp"
    ,"bmpsuite_good/pal1.bmp"
    ,"bmpsuite_good/pal8gs.bmp"
    ,"bmpsuite_good/pal8w124.bmp"
    ,"bmpsuite_good/rgb24.bmp"
    ,"bmpsuite_good/pal1wb.bmp"
    ,"bmpsuite_good/pal8nonsquare.bmp"
    ,"bmpsuite_good/pal8w125.bmp"
    ,"bmpsuite_good/rgb24pal.bmp"
    ,"bmpsuite_good/pal4.bmp"
    ,"bmpsuite_good/pal8os2.bmp"
    ,"bmpsuite_good/pal8w126.bmp"
    ,"bmpsuite_good/rgb32bf.bmp"
    ,"bmpsuite_good/pal4gs.bmp"
    ,"bmpsuite_good/pal8rle.bmp"
    ,"bmpsuite_good/rgb16-565.bmp"
    ,"bmpsuite_good/rgb32bfdef.bmp"
    ,"bmpsuite_good/pal4rle.bmp"
    ,"bmpsuite_good/pal8topdown.bmp"
    ,"bmpsuite_good/rgb16-565pal.bmp"
    ,"bmpsuite_good/rgb32.bmp"
    ,"bmpsuite_good/pal8-0.bmp"
    ,"bmpsuite_good/pal8v4.bmp"
    ,"bmpsuite_good/rgb16bfdef.bmp"
    ]

eitherDo :: Either String (IO ()) -> IO ()
eitherDo (Left str) = putStrLn str
eitherDo (Right action) = action

imgToImg :: FilePath -> IO ()
imgToImg path = do
    rez <- readImageWithMetadata path
    case rez of
        Right (ImageYCbCr8 img, met) -> do
            let rgb = convertImage img :: Image PixelRGB8
                bmp = encodeBitmap rgb
            putStrLn $ "YCbCr : " ++ path
            print met
            putStrLn "-> BMP"
            L.writeFile (path ++ "._fromYCbCr8.bmp") bmp

        Right (ImageRGB8 img, met) -> do
            let bmp = encodeBitmap img
            putStrLn $ "RGB8 : " ++ path
            print met
            putStrLn "-> BMP"
            L.writeFile (path ++ "._fromRGB8.bmp") bmp

        Right (ImageRGBA8 img, met) -> do
            let bmp = encodeBitmap img
            putStrLn $ "RGBA8 : " ++ path
            print met
            putStrLn "-> BMP"
            L.writeFile (path ++ "._fromRGBA8.bmp") bmp

        Right (ImageY8 img, met) -> do
            let bmp = encodeBitmap img
            putStrLn $ "Y8 : " ++ path
            print met
            putStrLn "-> BMP"
            L.writeFile (path ++ "._fromY8.bmp") bmp

        Right (ImageYA8 img, met) -> do
            let bmp = encodeBitmap $ (promoteImage img :: Image PixelRGB8)
            putStrLn $ "YA8 : " ++ path
            print met
            putStrLn "-> BMP"
            L.writeFile (path ++ "._fromYA8.bmp") bmp

        Left err ->
            putStrLn $ "Error loading " ++ path ++ " " ++ err

toStandardDef :: Image PixelRGBF -> Image PixelRGB8
toStandardDef img = pixelMap pixelConverter img
  where fix = truncate . (254 *) . max 0.0 . min 1.0
        pixelConverter (PixelRGBF rf gf bf) = PixelRGB8 r g b
          where r = fix rf
                g = fix gf
                b = fix bf

fromFloatTo32 :: Image PixelF -> Image Pixel32
fromFloatTo32 = pixelMap (\f -> floor (f * (fromIntegral (maxBound :: Word32))))

fromFloatTo16 :: Image PixelF -> Image Pixel16
fromFloatTo16 = pixelMap (\f -> floor (f * (fromIntegral (maxBound :: Word16))))

from32ToFloat :: Image Pixel32 -> Image PixelF
from32ToFloat = pixelMap (\w -> fromIntegral w / 4294967296.0)

from32To16 :: Image Pixel32 -> Image Pixel16
from32To16 = pixelMap (fromIntegral . (`unsafeShiftR` 16))

testSuite :: IO ()
testSuite = do
    mapM_ (imgToImg . (("tests" </> "bmp") </>)) bmpValidTests

myMain :: IO ()
myMain = do
    prog <- liftM (map toLower) getProgName
    args <- getArgs
    case args of
        ("test":_) -> testSuite
        [] | "imagetest"      `isInfixOf` prog -> testSuite
        _ -> pure ()

main :: IO ()
main = myMain

