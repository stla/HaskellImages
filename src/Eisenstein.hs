module Eisenstein
    (myImage
    , funColor
    , colorFun
    , colorMap
    , saveImage
    ) where
import Data.Complex ( Complex(..), magnitude, realPart, imagPart)
import Graphics.Image
    ( makeImageR, writeImage, RGB, Image, Pixel(PixelRGB), Pixel(PixelHSI), VU(..), toPixelRGB, ToRGB (toPixelRGB) )
import ColorMap (colorMap, colorMap2)
import Math.Eisenstein (eisensteinE6)


width :: Int 
width = 512
height :: Int 
height = 512
subdivisions :: Double
subdivisions = 1024

width' :: Double
width' = fromIntegral width
height' :: Double
height' = fromIntegral height


colorFun :: (Int, Int) -> Pixel RGB Double
colorFun (i, j) = 
    let i' = width' * fromIntegral i * 2 * subdivisions
        j' = height' * fromIntegral j * 2 * subdivisions
        z = i' :+ j' 
        (r, g, b) = (colorMap) (eisensteinE6 z) 
    in
    PixelRGB r g b

myImage :: ((Int, Int) -> Pixel RGB Double) -> (Int, Int) -> Image VU RGB Double
myImage thefun (m, n) = makeImageR VU (m, n) thefun

funColor :: Image VU RGB Double
funColor = myImage colorFun (width, height)

saveImage :: FilePath -> IO ()
saveImage file = writeImage ("images/" ++ file) funColor

funrgb :: Double -> Double -> Pixel RGB Double
funrgb i j = 
      let ij = (i / 512) :+ (j / 512)
          modulus = magnitude ij
          valid = (modulus < 1) && (realPart ij >= 0) 
          (r, g, b) = 
             if valid 
            then colorMap (eisensteinE6 ij)
            else (0, 0, 0)
    in PixelRGB r g b