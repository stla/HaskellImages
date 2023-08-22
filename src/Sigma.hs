module Sigma
    (colorFun
    , myImage
    , colorMap
    , funColor
    , saveImage
    , saveImage2
    ) where
import Data.Complex ( Complex(..) )
import Graphics.Image
    ( makeImageR, writeImage, RGB, Image, Pixel(PixelRGB), VU(..), Pixel(PixelHSI), toPixelRGB )
import ColorMap (colorMap, colorMap2)
import Math.Weierstrass (weierstrassSigma, ellipticInvariants)

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
g2g3 = ellipticInvariants (0.5 :+ 0) (0 :+ 0.5)

colorFun :: (Int, Int) -> Pixel RGB Double
colorFun (i, j) = 
    let i' = xlimitLwr' + fromIntegral i / width' * (xlimitUpr' - xlimitLwr')
        j' = ylimitLwr' + fromIntegral j / height' * (ylimitUpr' - ylimitLwr')
        z = i' :+ j' 
        wz = weierstrassSigma z (fst g2g3) (snd g2g3)
        (r, g, b) = colorMap wz
    in
    PixelRGB r g b

colorFun2 :: (Int, Int) -> Pixel RGB Double
colorFun2 (i, j) =
    let i' = xlimitLwr' + fromIntegral i / width' * (xlimitUpr' - xlimitLwr')
        j' = ylimitLwr' + fromIntegral j / height' * (ylimitUpr' - ylimitLwr')
        z = i' :+ j' 
        wz = weierstrassSigma z (fst g2g3) (snd g2g3)
        (h, s, intensity) = colorMap2 wz
    in
    toPixelRGB (PixelHSI h s intensity)

myImage :: ((Int, Int) -> Pixel RGB Double) -> (Int, Int) -> Image VU RGB Double
myImage thefun (w, h) = makeImageR VU (w, h) thefun

funColor :: Image VU RGB Double
funColor = myImage colorFun (height, width)
--  (height + (ylimitUpr - ylimitLwr), width + (xlimitUpr - xlimitLwr))

funColor2 :: Image VU RGB Double
funColor2 = myImage colorFun2 (height, width)

saveImage :: FilePath -> IO ()
saveImage file = writeImage ("images/" ++ file) funColor

saveImage2 :: FilePath -> IO ()
saveImage2 file = writeImage ("images/" ++ file) funColor2