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

xlimitLwr :: Double
xlimitLwr = 1.0
xlimitUpr :: Double
xlimitUpr = 1.0

ylimitLwr :: Double
ylimitLwr = 1.0
ylimitUpr :: Double
ylimitUpr = 1.0

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

g2g3 :: (Complex Double, Complex Double)
g2g3 = ellipticInvariants (0.5 :+ 0) (0 :+ 0.5)

colorFun :: (Int, Int) -> Pixel RGB Double
colorFun (i, j) = 
    let i' = width' * fromIntegral i * (xlimitUpr - xlimitLwr) / subdivisions
    in
    let j' = height' * fromIntegral j * (ylimitUpr - ylimitLwr) / subdivisions
    in 
    let z = i' :+ j' 
    in
    let wz = weierstrassSigma z (fst g2g3) (snd g2g3)
    in
    let (rr, gg, bb) = colorMap wz
    in
    PixelRGB rr gg bb

myImage :: ((Int, Int) -> Pixel RGB Double) -> (Int, Int) -> Image VU RGB Double
myImage thefun (w, h) = makeImageR VU (w, h) thefun

funColor :: Image VU RGB Double
funColor = myImage colorFun (width, height)

saveImage :: FilePath -> IO ()
saveImage file = writeImage ("images/" ++ file) funColor