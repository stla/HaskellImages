module ColorFun
    ( saveImage
    , saveImage2
    , saveImage3
    , saveImage4
    , saveImage'
    , saveImage2'
    , saveImage3'
    , saveImage4' ) 
    where
import Data.Complex ( Complex(..) )
import Graphics.Image
    ( makeImageR, writeImage, RGB, Image, Pixel(PixelRGB), VU(..), Pixel(PixelHSI), toPixelRGB )
import ColorMap (colorMap, colorMap2, colorMap3, colorMap4)
import qualified Data.Array as A (Array, (!))

type Func = Complex Double -> Maybe (Complex Double)

increments :: (Int, Int) -> (Double, Double) -> (Double, Double) 
           -> ((Int, Int) -> (Double, Double))
increments (w, h) (xlwr, xupr) (ylwr, yupr) = func
    where
    width', height' :: Double
    width' = fromIntegral w
    height' = fromIntegral h
    func (i, j) = 
        ( xlwr + fromIntegral i / width' * (xupr - xlwr)
        , ylwr + fromIntegral j / height' * (yupr - ylwr)
        )

colorFunArray :: A.Array (Int, Int) (Complex Double) 
                    -> (Int, Int) -> Pixel RGB Double
colorFunArray arr (i, j) = 
    let z = arr A.! (i, j)
        (r, g, b) = colorMap (Just z)
    in
    PixelRGB r g b

colorFunArray2 :: A.Array (Int, Int) (Complex Double) 
                    -> (Int, Int) -> Pixel RGB Double
colorFunArray2 arr (i, j) = 
    let z = arr A.! (i, j)
        (hue, s, intensity) = colorMap2 (Just z)
    in
    toPixelRGB (PixelHSI hue s intensity)

colorFun :: Func -> (Int, Int) -> (Double, Double) -> (Double, Double) 
         -> (Int, Int) -> Pixel RGB Double
colorFun func (w, h) (xlwr, xupr) (ylwr, yupr) (i, j) = 
    let (i', j') = increments (w, h) (xlwr, xupr) (ylwr, yupr) (i, j)
        z = i' :+ j' 
        (r, g, b) = colorMap (func z)
    in
    PixelRGB r g b


colorFun2 :: Func -> (Int, Int) -> (Double, Double) -> (Double, Double) 
          -> (Int, Int) -> Pixel RGB Double
colorFun2 func (w, h) (xlwr, xupr) (ylwr, yupr) (i, j) = 
    let (i', j') = increments (w, h) (xlwr, xupr) (ylwr, yupr) (i, j)
        z = i' :+ j' 
        (hue, s, intensity) = colorMap2 (func z)
    in
    toPixelRGB (PixelHSI hue s intensity)


colorFun3 :: Func -> Double -> Double -> (Int, Int) 
          -> (Double, Double) -> (Double, Double) -> (Int, Int) -> Pixel RGB Double
colorFun3 func s n (w, h) (xlwr, xupr) (ylwr, yupr) (i, j) = 
    let (i', j') = increments (w, h) (xlwr, xupr) (ylwr, yupr) (i, j)
        z = i' :+ j' 
        (r, g, b) = colorMap3 (func z) s n
    in
    PixelRGB r g b

colorFunArray3 :: A.Array (Int, Int) (Complex Double) 
                    -> Double -> Double
                    -> (Int, Int) -> Pixel RGB Double
colorFunArray3 arr s n (i, j) = 
    let z = arr A.! (i, j)
        (r, g, b) = colorMap3 (Just z) s n
    in
    PixelRGB r g b


colorFun4 :: Func -> (Int, Int) -> (Double, Double) -> (Double, Double) 
          -> (Int, Int) -> Pixel RGB Double
colorFun4 func (w, h) (xlwr, xupr) (ylwr, yupr) (i, j) = 
    let (i', j') = increments (w, h) (xlwr, xupr) (ylwr, yupr) (i, j)
        z = i' :+ j' 
        (r, g, b) = colorMap4 (func z)
    in
    PixelRGB r g b

colorFunArray4 :: A.Array (Int, Int) (Complex Double) 
                    -> (Int, Int) -> Pixel RGB Double
colorFunArray4 arr (i, j) = 
    let z = arr A.! (i, j)
        (r, g, b) = colorMap4 (Just z)
    in
    PixelRGB r g b



myImage :: ((Int, Int) -> Pixel RGB Double) -> (Int, Int) 
        -> Image VU RGB Double
myImage thefun (w, h) = makeImageR VU (w, h) thefun


funArrayColor :: A.Array (Int, Int) (Complex Double) -> (Int, Int) 
                 -> Image VU RGB Double
funArrayColor arr (w, h) = 
    myImage (colorFunArray arr) (h, w) 

funArrayColor2 :: A.Array (Int, Int) (Complex Double) -> (Int, Int) 
                 -> Image VU RGB Double
funArrayColor2 arr (w, h) = 
    myImage (colorFunArray2 arr) (h, w) 

funColor :: Func -> (Int, Int) -> (Double, Double) -> (Double, Double) 
         -> Image VU RGB Double
funColor func (w, h) (xlwr, xupr) (ylwr, yupr) = 
    myImage (colorFun func (w, h) (xlwr, xupr) (ylwr, yupr)) (h, w) 


funColor2 :: Func -> (Int, Int) -> (Double, Double) -> (Double, Double) 
          -> Image VU RGB Double
funColor2 func (w, h) (xlwr, xupr) (ylwr, yupr) = 
    myImage (colorFun2 func (w, h) (xlwr, xupr) (ylwr, yupr)) (h, w) 


funColor3 :: Func -> Double -> Double -> (Int, Int) 
          -> (Double, Double) -> (Double, Double) -> Image VU RGB Double
funColor3 func s n (w, h) (xlwr, xupr) (ylwr, yupr) = 
    myImage (colorFun3 func s n (w, h) (xlwr, xupr) (ylwr, yupr)) (h, w) 

funArrayColor3 :: A.Array (Int, Int) (Complex Double) 
                 -> Double -> Double -> (Int, Int) 
                 -> Image VU RGB Double
funArrayColor3 arr s n (w, h) = 
    myImage (colorFunArray3 arr s n) (h, w) 

funColor4 :: Func -> (Int, Int) -> (Double, Double) -> (Double, Double) 
          -> Image VU RGB Double
funColor4 func (w, h) (xlwr, xupr) (ylwr, yupr) = 
    myImage (colorFun4 func (w, h) (xlwr, xupr) (ylwr, yupr)) (h, w) 

funArrayColor4 :: A.Array (Int, Int) (Complex Double) -> (Int, Int) 
                 -> Image VU RGB Double
funArrayColor4 arr (w, h) = 
    myImage (colorFunArray4 arr) (h, w) 



saveImage :: Func -> (Int, Int) -> (Double, Double) -> (Double, Double) 
          -> FilePath -> IO ()
saveImage func (w, h) (xlwr, xupr) (ylwr, yupr) file = 
    writeImage ("images/" ++ file) 
               (funColor func (w, h) (xlwr, xupr) (ylwr, yupr))


saveImage' :: A.Array (Int, Int) (Complex Double) -> (Int, Int) 
                -> FilePath -> IO ()
saveImage' arr (w, h) file = 
    writeImage ("images/" ++ file) 
               (funArrayColor arr (w, h))

saveImage2' :: A.Array (Int, Int) (Complex Double) -> (Int, Int) 
                -> FilePath -> IO ()
saveImage2' arr (w, h) file = 
    writeImage ("images/" ++ file) 
               (funArrayColor2 arr (w, h))

saveImage3' :: A.Array (Int, Int) (Complex Double) 
                -> Double -> Double -> (Int, Int) 
                -> FilePath -> IO ()
saveImage3' arr s n (w, h) file = 
    writeImage ("images/" ++ file) 
               (funArrayColor3 arr s n (w, h))

saveImage4' :: A.Array (Int, Int) (Complex Double) -> (Int, Int) 
                -> FilePath -> IO ()
saveImage4' arr (w, h) file = 
    writeImage ("images/" ++ file) 
               (funArrayColor4 arr (w, h))

saveImage2 :: Func -> (Int, Int) -> (Double, Double) -> (Double, Double) 
           -> FilePath -> IO ()
saveImage2 func (w, h) (xlwr, xupr) (ylwr, yupr) file = 
    writeImage ("images/" ++ file) 
               (funColor2 func (w, h) (xlwr, xupr) (ylwr, yupr))

saveImage3 :: Func -> Double -> Double -> (Int, Int) 
           -> (Double, Double) -> (Double, Double) -> FilePath -> IO ()
saveImage3 func s n (w, h) (xlwr, xupr) (ylwr, yupr) file = 
    writeImage ("images/" ++ file) 
               (funColor3 func s n (w, h) (xlwr, xupr) (ylwr, yupr))


saveImage4 :: Func -> (Int, Int) -> (Double, Double) -> (Double, Double) 
           -> FilePath -> IO ()
saveImage4 func (w, h) (xlwr, xupr) (ylwr, yupr) file = 
    writeImage ("images/" ++ file) 
               (funColor4 func (w, h) (xlwr, xupr) (ylwr, yupr))
