module Util where

import Control.Arrow( first )
import Control.DeepSeq( NFData, deepseq )
import qualified Control.Exception as Exc ( catch, IOException )
import qualified Data.ByteString as B

import Codec.Picture.Bitmap
import Codec.Picture.Metadata
import Codec.Picture.Types

eitherLoad :: c -> [(String, c -> Either String b)] -> Either String b
eitherLoad v = inner ""
    where inner errAcc [] = Left $ "Cannot load file\n" ++ errAcc
          inner errAcc ((hdr, f) : rest) = case f v of
                Left  err  -> inner (errAcc ++ hdr ++ " " ++ err ++ "\n") rest
                Right rez  -> Right rez

withImageDecoder decoder path = Exc.catch doit
                    (\e -> return . Left $ show (e :: Exc.IOException))
    where doit = force . decoder <$> get
          get = B.readFile path
          -- force appeared in deepseq 1.3, Haskell Platform

          -- provides 1.1

          force x = x `deepseq` x


decodeImageWithPaletteAndMetadata :: B.ByteString -> Either String (PalettedImage, Metadatas)
decodeImageWithPaletteAndMetadata str = eitherLoad str [ ("Bitmap", decodeBitmapWithPaletteAndMetadata) ]

decodeImageWithMetadata :: B.ByteString -> Either String (DynamicImage, Metadatas)
decodeImageWithMetadata =
    fmap (first palettedToTrueColor) . decodeImageWithPaletteAndMetadata


readImageWithMetadata :: FilePath -> IO (Either String (DynamicImage, Metadatas))
readImageWithMetadata = withImageDecoder decodeImageWithMetadata