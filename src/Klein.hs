module Klein
    (colorFun
    , saveImage
    , saveImage3
    ) where
import Data.Complex ( Complex(..), magnitude )
import Graphics.Image
    ( makeImageR, writeImage, RGB, Image, Pixel(PixelRGB), VU(..), toPixelRGB, ToRGB (toPixelRGB) )
import ColorMap (colorMap, colorMap3)
import Math.Eisenstein (kleinJ)

xlimitLwr :: Int
xlimitLwr = -1
xlimitUpr :: Int
xlimitUpr = 1

ylimitLwr :: Int
ylimitLwr = -1
ylimitUpr :: Int
ylimitUpr = 1

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

psi :: Complex Double -> Complex Double
psi z = im + 2*im / (im/z - 1)
  where
    im = 0 :+ 1

colorFun :: (Int, Int) -> Pixel RGB Double
colorFun (i, j) = 
    let i' = xlimitLwr' + fromIntegral i / width' * (xlimitUpr' - xlimitLwr')
        j' = ylimitLwr' + fromIntegral j / height' * (ylimitUpr' - ylimitLwr')
        z = i' :+ j' 
        (r, g ,b) = if magnitude z > 0.96 
            then (0, 0, 0)
            else if j < 0  
                then colorMap (-1 / psi (kleinJ z))
                else colorMap (psi (kleinJ z))
    in
    PixelRGB r g b

colorFun3 :: Double -> Double -> (Int, Int) -> Pixel RGB Double
colorFun3 s r (i, j) = 
    let i' = xlimitLwr' + fromIntegral i / width' * (xlimitUpr' - xlimitLwr')
        j' = ylimitLwr' + fromIntegral j / height' * (ylimitUpr' - ylimitLwr')
        z = i' :+ j' 
        (red, green ,blue) = if magnitude z > 0.96 
            then (0, 0, 0)
            else if j < 0  
                then colorMap3 (-1 / psi (kleinJ z)) s r
                else colorMap3 (psi (kleinJ z)) s r
    in
    PixelRGB red green blue

myImage :: ((Int, Int) -> Pixel RGB Double) -> (Int, Int) -> Image VU RGB Double
myImage thefun (m, n) = makeImageR VU (m, n) thefun

funColor :: Image VU RGB Double
funColor = myImage colorFun (width, height)

funColor3 :: Double -> Double -> Image VU RGB Double
funColor3 s r = myImage (colorFun3 s r) (height, width)

saveImage :: FilePath -> IO ()
saveImage file = writeImage ("images/" ++ file) funColor

saveImage3 :: FilePath -> IO ()
saveImage3 file = writeImage ("images/" ++ file) (funColor3 0.9 3)
