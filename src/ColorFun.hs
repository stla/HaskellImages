module ColorFun
    ( saveImage
    , saveImage2
    , saveImage3
    , saveImage4
    ) where
import Data.Complex ( Complex(..) )
import Graphics.Image
    ( makeImageR, writeImage, RGB, Image, Pixel(PixelRGB), VU(..), Pixel(PixelHSI), toPixelRGB )
import ColorMap (colorMap, colorMap2, colorMap3, colorMap4)

type Func = Complex Double -> Complex Double

increments :: (Int, Int) -> (Double, Double) -> (Double, Double) -> ((Int, Int) -> (Double, Double))
increments (w, h) (xlwr, xupr) (ylwr, yupr) = func
    where
    width', height' :: Double
    width' = fromIntegral w
    height' = fromIntegral h
    func (i, j) = 
        ( xlwr + fromIntegral i / width' * (xupr - xlwr)
        , ylwr + fromIntegral j / height' * (yupr - ylwr)
        )


colorFun :: Func -> (Int, Int) -> (Double, Double) -> (Double, Double) -> (Int, Int) -> Pixel RGB Double
colorFun func (w, h) (xlwr, xupr) (ylwr, yupr) (i, j) = 
    let (i', j') = increments (w, h) (xlwr, xupr) (ylwr, yupr) (i, j)
        z = i' :+ j' 
        (r, g, b) = colorMap (func z)
    in
    PixelRGB r g b

colorFun2 :: Func -> (Int, Int) -> (Double, Double) -> (Double, Double) -> (Int, Int) -> Pixel RGB Double
colorFun2 func (w, h) (xlwr, xupr) (ylwr, yupr) (i, j) = 
    let (i', j') = increments (w, h) (xlwr, xupr) (ylwr, yupr) (i, j)
        z = i' :+ j' 
        (hue, s, intensity) = colorMap2 (func z)
    in
    toPixelRGB (PixelHSI hue s intensity)

colorFun3 :: Func -> Double -> Double -> (Int, Int) -> (Double, Double) -> (Double, Double) -> (Int, Int) -> Pixel RGB Double
colorFun3 func s n (w, h) (xlwr, xupr) (ylwr, yupr) (i, j) = 
    let (i', j') = increments (w, h) (xlwr, xupr) (ylwr, yupr) (i, j)
        z = i' :+ j' 
        (r, g, b) = colorMap3 (func z) s n
    in
    PixelRGB r g b

colorFun4 :: Func -> (Int, Int) -> (Double, Double) -> (Double, Double) -> (Int, Int) -> Pixel RGB Double
colorFun4 func (w, h) (xlwr, xupr) (ylwr, yupr) (i, j) = 
    let (i', j') = increments (w, h) (xlwr, xupr) (ylwr, yupr) (i, j)
        z = i' :+ j' 
        (r, g, b) = colorMap4 (func z)
    in
    PixelRGB r g b


myImage :: ((Int, Int) -> Pixel RGB Double) -> (Int, Int) -> Image VU RGB Double
myImage thefun (w, h) = makeImageR VU (w, h) thefun

funColor :: Func -> (Int, Int) -> (Double, Double) -> (Double, Double) -> Image VU RGB Double
funColor func (w, h) (xlwr, xupr) (ylwr, yupr) = 
    myImage (colorFun func (w, h) (xlwr, xupr) (ylwr, yupr)) (h, w) 

funColor2 :: Func -> (Int, Int) -> (Double, Double) -> (Double, Double) -> Image VU RGB Double
funColor2 func (w, h) (xlwr, xupr) (ylwr, yupr) = 
    myImage (colorFun2 func (w, h) (xlwr, xupr) (ylwr, yupr)) (h, w) 

funColor3 :: Func -> Double -> Double -> (Int, Int) -> (Double, Double) -> (Double, Double) -> Image VU RGB Double
funColor3 func s n (w, h) (xlwr, xupr) (ylwr, yupr) = 
    myImage (colorFun3 func s n (w, h) (xlwr, xupr) (ylwr, yupr)) (h, w) 

funColor4 :: Func -> (Int, Int) -> (Double, Double) -> (Double, Double) -> Image VU RGB Double
funColor4 func (w, h) (xlwr, xupr) (ylwr, yupr) = 
    myImage (colorFun4 func (w, h) (xlwr, xupr) (ylwr, yupr)) (h, w) 

saveImage :: Func -> (Int, Int) -> (Double, Double) -> (Double, Double) -> FilePath -> IO ()
saveImage func (w, h) (xlwr, xupr) (ylwr, yupr) file = 
    writeImage ("images/" ++ file) (funColor func (w, h) (xlwr, xupr) (ylwr, yupr))

saveImage2 :: Func -> (Int, Int) -> (Double, Double) -> (Double, Double) -> FilePath -> IO ()
saveImage2 func (w, h) (xlwr, xupr) (ylwr, yupr) file = 
    writeImage ("images/" ++ file) (funColor2 func (w, h) (xlwr, xupr) (ylwr, yupr))

saveImage3 :: Func -> Double -> Double -> (Int, Int) -> (Double, Double) -> (Double, Double) -> FilePath -> IO ()
saveImage3 func s n (w, h) (xlwr, xupr) (ylwr, yupr) file = 
    writeImage ("images/" ++ file) (funColor3 func s n (w, h) (xlwr, xupr) (ylwr, yupr))

saveImage4 :: Func -> (Int, Int) -> (Double, Double) -> (Double, Double) -> FilePath -> IO ()
saveImage4 func (w, h) (xlwr, xupr) (ylwr, yupr) file = 
    writeImage ("images/" ++ file) (funColor4 func (w, h) (xlwr, xupr) (ylwr, yupr))
