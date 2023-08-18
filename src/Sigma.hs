module Sigma
    (colorFun
    , myImage
    , colorMap
    , funColor
    , saveImage
    ) where
import Data.Complex ( Complex(..) )
import Graphics.Image
    ( makeImageR, writeImage, RGB, Image, Pixel(PixelRGB), VU(..) )
import ColorMap (colorMap)
import Math.Weierstrass (weierstrassSigma, ellipticInvariants)

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
    let i' = fromIntegral i / width' - 1
    in
    let j' = fromIntegral j / height' - 1
    in 
    let z = i' :+ j' 
    in
    let wz = weierstrassSigma z (fst g2g3) (snd g2g3)
    in
    let (rr, gg, bb) = colorMap wz
    in
    PixelRGB rr gg bb

myImage :: ((Int, Int) -> Pixel RGB Double) -> (Int, Int) -> Image VU RGB Double
myImage thefun (m, n) = makeImageR VU (m, n) thefun

funColor :: Image VU RGB Double
funColor = myImage colorFun (1024, 1024)

saveImage :: IO ()
saveImage = writeImage "images/mysigma.png" funColor
