module Zeta
    (colorFun
    , myImage
    , zetaColor
    , saveImage
    ) where
import Data.Complex ( Complex(..), realPart, imagPart, magnitude )
import Graphics.Image
    ( makeImageR, writeImage, RGB, Image, Pixel(PixelRGB), VU(..) )
import Math.Weierstrass (weierstrassZeta)

colorMap :: Complex Double -> (Double, Double, Double)
colorMap z = (realPart z, imagPart z, magnitude z)

colorFun :: (Int, Int) -> Pixel RGB Double
colorFun (i, j) = 
    let i' = (fromIntegral i) / 512
    in
    let j' = (fromIntegral j) / 512
    in 
    let z = i' :+ j' 
    in
    let wz =  weierstrassZeta z (1 :+ 1) (2 :+ 2)
    in
    let (aa, bb, blue) = colorMap wz
    in
    PixelRGB i' j' blue

myImage :: ((Int, Int) -> Pixel RGB Double) -> (Int, Int) -> Image VU RGB Double
myImage thefun (m, n) = makeImageR VU (m, n) thefun

zetaColor :: Image VU RGB Double
zetaColor = myImage colorFun (512, 512)

saveImage :: IO ()
saveImage = writeImage "images/myzeta.png" zetaColor
