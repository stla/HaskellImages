module Zeta
    (colorFun
    , colorFun2
    , myImage
    , funColor
    , funColor2
    , saveImage
    , saveImage2
    , saveImage3
    ) where
import Data.Complex ( Complex(..) )
import Graphics.Image
    ( makeImageR, writeImage, RGB, Image, Pixel(PixelRGB), Pixel(PixelHSI), VU(..), toPixelRGB, ToRGB (toPixelRGB) )
import ColorMap (colorMap, colorMap2, colorMap3)
import Math.Weierstrass (weierstrassZeta, ellipticInvariants)

xlimitLwr :: Int
xlimitLwr = -200
xlimitUpr :: Int
xlimitUpr = 200

ylimitLwr :: Int
ylimitLwr = -200
ylimitUpr :: Int
ylimitUpr = 200

xlimitLwr' :: Double
xlimitLwr' = fromIntegral xlimitLwr
xlimitUpr' :: Double
xlimitUpr' = fromIntegral xlimitUpr
ylimitLwr' :: Double
ylimitLwr' = fromIntegral ylimitLwr
ylimitUpr' :: Double
ylimitUpr' = fromIntegral ylimitUpr

width :: Int 
width = 512
height :: Int 
height = 512

width' :: Double
width' = fromIntegral width
height' :: Double
height' = fromIntegral height


g2g3 :: (Complex Double, Complex Double)
g2g3 = ellipticInvariants (0.1 :+ 0) (0 :+ 0.1)

g2, g3 :: Complex Double
g2 = fst g2g3
g3 = snd g2g3



colorFun :: (Int, Int) -> Pixel RGB Double
colorFun (i, j) = 
    let i' = xlimitLwr' + fromIntegral i / width' * (xlimitUpr' - xlimitLwr')
        j' = ylimitLwr' + fromIntegral j / height' * (ylimitUpr' - ylimitLwr')
        z = i' :+ j' 
        wz = weierstrassZeta z g2 g3
        (r, g, b) = colorMap wz
    in
    PixelRGB r g b

colorFun2 :: (Int, Int) -> Pixel RGB Double
colorFun2 (i, j) =
    let i' = xlimitLwr' + fromIntegral i / width' * (xlimitUpr' - xlimitLwr')
        j' = ylimitLwr' + fromIntegral j / height' * (ylimitUpr' - ylimitLwr')
        z = i' :+ j' 
        wz = weierstrassZeta z g2 g3
        (h, s, intensity) = colorMap2 wz
    in
    toPixelRGB (PixelHSI h s intensity)

colorFun3 :: Double -> Double -> (Int, Int) -> Pixel RGB Double
colorFun3 s r (i, j) = 
    let i' = xlimitLwr' + fromIntegral i / width' * (xlimitUpr' - xlimitLwr')
        j' = ylimitLwr' + fromIntegral j / height' * (ylimitUpr' - ylimitLwr')
        z = i' :+ j' 
        wz = weierstrassZeta z g2 g3
        (red, green, blue) = colorMap3 wz s r
    in
    PixelRGB red green blue

myImage :: ((Int, Int) -> Pixel RGB Double) -> (Int, Int) -> Image VU RGB Double
myImage thefun (m, n) = makeImageR VU (m, n) thefun

funColor :: Image VU RGB Double
funColor = myImage colorFun (width, height)

funColor2 :: Image VU RGB Double
funColor2 = myImage colorFun2 (height, width)

funColor3 :: Double -> Double -> Image VU RGB Double
funColor3 s r = myImage (colorFun3 s r) (height, width)

saveImage :: FilePath -> IO ()
saveImage file = writeImage ("images/" ++ file) funColor

saveImage2 :: FilePath -> IO ()
saveImage2 file = writeImage ("images/" ++ file) funColor2

saveImage3 :: FilePath -> IO ()
saveImage3 file = writeImage ("images/" ++ file) (funColor3 80 4)
