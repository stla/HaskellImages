module Zeta
    (colorFun
    , colorFun2
    , myImage
    , myImage2
    , funColor
    , funColor2
    , saveImage
    , saveImage2
    ) where
import Data.Complex ( Complex(..) )
import Graphics.Image
    ( makeImageR, writeImage, RGB, Image, Pixel(PixelRGB), Pixel(PixelHSI), VU(..), toPixelRGB, ToRGB (toPixelRGB) )
import ColorMap (colorMap, colorMap2)
import Math.Weierstrass (weierstrassZeta, ellipticInvariants)

width :: Int 
width = 768
height :: Int 
height = 768

width' :: Double
width' = fromIntegral width
height' :: Double
height' = fromIntegral height

g2g3 :: (Complex Double, Complex Double)
g2g3 = ellipticInvariants (0.5 :+ 0) (0 :+ 0.5)

colorFun :: (Int, Int) -> Pixel RGB Double
colorFun (i, j) = 
    let i' = fromIntegral i / width' - 0.5
    in
    let j' = fromIntegral j / height' - 0.5
    in 
    let z = i' :+ j' 
    in
    let wz =  weierstrassZeta z (fst g2g3) (snd g2g3)
    in
    let (rr, gg, bb) = colorMap wz
    in
    PixelRGB rr gg bb

colorFun2 :: (Int, Int) -> Pixel RGB Double
colorFun2 (i, j) = 
    let i' = fromIntegral i / width' - 0.5
    in
    let j' = fromIntegral j / height' - 0.5
    in 
    let z = i' :+ j' 
    in
    let wz =  weierstrassZeta z (fst g2g3) (snd g2g3)
    in
    let (hh, ss, ii) = colorMap2 wz
    in
    toPixelRGB (PixelHSI hh ss ii)

myImage :: ((Int, Int) -> Pixel RGB Double) -> (Int, Int) -> Image VU RGB Double
myImage thefun (m, n) = makeImageR VU (m, n) thefun

funColor :: Image VU RGB Double
funColor = myImage colorFun (width, height)

saveImage :: FilePath -> IO ()
saveImage file = writeImage ("images/" ++ file) funColor

myImage2 :: ((Int, Int) -> Pixel RGB Double) -> (Int, Int) -> Image VU RGB Double
myImage2 thefun (m, n) = makeImageR VU (m, n) thefun

funColor2 :: Image VU RGB Double
funColor2 = myImage2 colorFun2 (width, height)

saveImage2 :: FilePath -> IO ()
saveImage2 file = writeImage ("images/" ++ file) funColor2
