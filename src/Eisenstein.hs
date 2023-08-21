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

modulusOK :: Complex Double -> Bool
modulusOK tau = 
    magnitude q < 0.85 && (realPart q > 0)
    where q = exp((0 :+ 1) * tau * pi)

colorFun :: (Int, Int) -> Pixel RGB Double
colorFun (i, j) = 
    let i' = width' * fromIntegral i * 2 / subdivisions
        j' = height' * fromIntegral j * 2 / subdivisions
        z = i' :+ j' 
        (r, g, b) = if modulusOK z 
            then (colorMap) (eisensteinE6 z)
            else (0, 0, 0)
    in
    PixelRGB r g b

myImage :: ((Int, Int) -> Pixel RGB Double) -> (Int, Int) -> Image VU RGB Double
myImage thefun (m, n) = makeImageR VU (m, n) thefun

funColor :: Image VU RGB Double
funColor = myImage colorFun (width, height)

saveImage :: FilePath -> IO ()
saveImage file = writeImage ("images/" ++ file) funColor


-- funrgb :: Double -> Double -> Pixel RGB Double
-- funrgb i j = 
--     let ij = (i / 512) :+ (j / 512)
--         valid = modulusOK ij
--         (r, g, b) = 
--             if valid 
--               then colorMap (eisensteinE6 ij)
--               else (0, 0, 0)
--     in PixelRGB r g b