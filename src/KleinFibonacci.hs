module KleinFibonacci
    (saveImage
    , saveImage3
    , saveImage4
    ) where
import Data.Complex ( Complex(..), magnitude )
import Graphics.Image
    ( makeImageR, writeImage, RGB, Image, Pixel(PixelRGB), VU(..) )
import ColorMap (colorMap, colorMap3, colorMap4)
import Math.Eisenstein (kleinJ)

xlimitLwr' :: Double
xlimitLwr' = -1
xlimitUpr' :: Double
xlimitUpr' = 1
ylimitLwr' :: Double
ylimitLwr' = -1
ylimitUpr' :: Double
ylimitUpr' = 1

width :: Int 
width = 1024
height :: Int 
height = 1024

width' :: Double
width' = fromIntegral width
height' :: Double
height' = fromIntegral height

psi :: Complex Double -> Complex Double
psi z = im + 2*im*z / (im - z)
  where
    im = 0 :+ 1

fibo :: Complex Double -> Complex Double
fibo z = z / (1 - z - z*z)

colorFun :: (Int, Int) -> Pixel RGB Double
colorFun (i, j) = 
    let i' = xlimitLwr' + fromIntegral i / width' * (xlimitUpr' - xlimitLwr')
        j' = ylimitLwr' + fromIntegral j / height' * (ylimitUpr' - ylimitLwr')
        z = i' :+ j' 
        (r, g ,b) = if magnitude z > 0.95
            then (0.1, 0.1, 0.1)
            else if j' < 0 
                then colorMap (fibo(kleinJ (-1 / psi z) / 1728)) 
                else colorMap (fibo(kleinJ (psi z) / 1728))
    in
    PixelRGB r g b

colorFun3 :: Double -> Double -> (Int, Int) -> Pixel RGB Double
colorFun3 s r (i, j) = 
    let i' = xlimitLwr' + fromIntegral i / width' * (xlimitUpr' - xlimitLwr')
        j' = ylimitLwr' + fromIntegral j / height' * (ylimitUpr' - ylimitLwr')
        z = i' :+ j' 
        (red, green ,blue) = if magnitude z > 0.95
            then (0.1, 0.1, 0.1)
            else if j' <= 0  
                then colorMap3 (fibo(kleinJ (-1 / psi z) / 1728)) s r 
                else colorMap3 (fibo(kleinJ (psi z) / 1728)) s r 
    in
    PixelRGB red green blue

colorFun4 :: (Int, Int) -> Pixel RGB Double
colorFun4 (i, j) = 
    let i' = xlimitLwr' + fromIntegral i / width' * (xlimitUpr' - xlimitLwr')
        j' = ylimitLwr' + fromIntegral j / height' * (ylimitUpr' - ylimitLwr')
        z = i' :+ j' 
        (r, g ,b) = if magnitude z > 0.95
            then (0.1, 0.1, 0.1)
            else if j' < 0 
                then colorMap4 (fibo(kleinJ (-1 / psi z) / 1728)) 
                else colorMap4 (fibo(kleinJ (psi z) / 1728)) 
    in
    PixelRGB r g b

myImage :: ((Int, Int) -> Pixel RGB Double) -> (Int, Int) -> Image VU RGB Double
myImage thefun (m, n) = makeImageR VU (m, n) thefun

funColor :: Image VU RGB Double
funColor = myImage colorFun (height, width)

funColor3 :: Double -> Double -> Image VU RGB Double
funColor3 s r = myImage (colorFun3 s r) (height, width)

funColor4 :: Image VU RGB Double
funColor4 = myImage colorFun4 (height, width)

saveImage :: FilePath -> IO ()
saveImage file = writeImage ("images/" ++ file) funColor

saveImage3 :: FilePath -> IO ()
saveImage3 file = writeImage ("images/" ++ file) (funColor3 0.9 2)

saveImage4 :: FilePath -> IO ()
saveImage4 file = writeImage ("images/" ++ file) funColor4
