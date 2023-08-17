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
import Math.Weierstrass (weierstrassZeta)

width :: Int 
width = 768
height :: Int 
height = 768

width' :: Double
width' = fromIntegral width
height' :: Double
height' = fromIntegral height

colorFun :: (Int, Int) -> Pixel RGB Double
colorFun (i, j) = 
    let i' = fromIntegral i / width' - 0.5
    in
    let j' = fromIntegral j / height' - 0.5
    in 
    let z = i' :+ j' 
    in
    let wz =  weierstrassZeta z (0 :+ 0.5) (1 :+ 0.5)
    in
    let (rr, gg, bb) = colorMap wz
    in
    PixelRGB rr gg bb

myImage :: ((Int, Int) -> Pixel RGB Double) -> (Int, Int) -> Image VU RGB Double
myImage thefun (m, n) = makeImageR VU (m, n) thefun

funColor :: Image VU RGB Double
funColor = myImage colorFun (width, height)

saveImage :: IO ()
saveImage = writeImage "images/myzeta2.png" funColor
