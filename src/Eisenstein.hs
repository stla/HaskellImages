module Eisenstein
    (myImage
    , funColor
    , colorFun
    , colorMap
    , saveImage
    ) where
import Data.Complex ( Complex(..), magnitude, realPart, imagPart)
import Graphics.Image
    ( makeImageR, writeImage, RGB, Image, Pixel(PixelRGB), VU(..))
import ColorMap (colorMap)
import Math.Eisenstein (eisensteinE4)


width :: Int 
width = 512
height :: Int 
height = 512
subdivisions :: Double
subdivisions = 1024

-- width' :: Double
-- width' = fromIntegral width
-- height' :: Double
-- height' = fromIntegral height

modulusOK :: Complex Double -> Bool
modulusOK tau = 
    --if(Mod(q) >= 0.99 || (Im(q) == 0 && Re(q) <= 0)) return(bkgcol)
    magnitude q < 0.8 && (imagPart q > 0 || realPart q < 0.8) --(and [(imagPart q == 0), (realPart q > 0), (realPart q < 0.85)])
    where q = exp((0 :+ 1) * tau * pi * 8)

colorFun :: (Int, Int) -> Pixel RGB Double
colorFun (i, j) = 
    let i' = fromIntegral i * 2 / subdivisions
        j' = fromIntegral j * 2 / subdivisions
        z = i' :+ j' 
        (r, g, b) = if modulusOK z 
            then colorMap (eisensteinE4 z)
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
--               then colorMap (eisensteinE4 ij)
--               else (0, 0, 0)
--     in PixelRGB r g b