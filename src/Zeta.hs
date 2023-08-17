module Zeta
    (colorFun
    , myImage
    , colorMap
    , zetaColor
    , saveImage
    ) where
import Data.Complex ( Complex(..), realPart, imagPart, magnitude )
import Graphics.Image
    ( makeImageR, writeImage, RGB, Image, Pixel(PixelRGB), VU(..) )
import Math.Weierstrass (weierstrassZeta)

-- :{
-- fracRem :: (RealFrac a, Integral b) => a -> b -> a
-- fracRem x y = x - fromIntegral (y * (truncate x `quot` y))
-- :}

modulo :: Double -> Int -> Double
modulo a p = 
    let p' = fromIntegral p
    in
    if a > 0 
        then a - fromIntegral(p * floor(a/p'))  
        else a - fromIntegral(p * ceiling(a/p'))

colorMap :: Complex Double -> (Double, Double, Double)
colorMap z = 
    let x = realPart z
    in
    let y = imagPart z
    in
    let a = atan2 y x 
    in 
    let r = modulo (magnitude z) 1
    in
    let g = abs(modulo (2*a) 1)
    in
    let b = abs(modulo (x*y) 1)
    in
    (
        (1.0 - cos(r-0.5))*8.0, 
        (1.0 - cos(g-0.5))*8.0, 
        (1.0 - cos(b-0.5))*8.0
    )

colorFun :: (Int, Int) -> Pixel RGB Double
colorFun (i, j) = 
    let i' = fromIntegral i / 768 - 0.5
    in
    let j' = fromIntegral j / 768 - 0.5
    in 
    let z = i' :+ j' 
    in
    let wz =  weierstrassZeta z (1 :+ 1) (2 :+ 2)
    in
    let (rr, gg, bb) = colorMap wz
    in
    PixelRGB rr gg bb

myImage :: ((Int, Int) -> Pixel RGB Double) -> (Int, Int) -> Image VU RGB Double
myImage thefun (m, n) = makeImageR VU (m, n) thefun

zetaColor :: Image VU RGB Double
zetaColor = myImage colorFun (768, 768)

saveImage :: IO ()
saveImage = writeImage "images/myzeta.png" zetaColor
