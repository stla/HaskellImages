module Zeta
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

myImage :: ((Int, Int) -> Pixel RGB Double) -> (Int, Int) -> Image VU RGB Double
myImage thefun (m, n) = makeImageR VU (m, n) thefun

funColor :: Image VU RGB Double
funColor = myImage colorFun (width, height)

saveImage :: FilePath -> IO ()
saveImage file = writeImage ("images/" ++ file) funColor
