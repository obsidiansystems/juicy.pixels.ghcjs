# juicy.pixels.ghcjs

[JuicyPixels](https://github.com/Twinside/Juicy.Pixels) contains many image processing modules. Some of them, such as the PNG module, depend on zlib, and hence don't work with GHCJS.

The `Codec.Picture.Bitmap` module, however, is written entirely in Haskell, and is GHCJS compatible. This repo is created around the Bitmap module.