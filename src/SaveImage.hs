module SaveImage
    ( saveImage
    , saveImage')
   where
import Data.Complex ( Complex(..) )
import Graphics.Image
    ( makeImageR, writeImage, RGB, Image, Pixel(PixelRGB), VU(..) )
import ColorMap (colorMap, colorMap2, colorMap3, colorMap4)
import qualified Data.Array as A (Array, (!))

type Func = Complex Double -> Maybe (Complex Double)
type ColorMap = Maybe (Complex Double) -> Pixel RGB Double
type Coloring = (Int, Int) -> Pixel RGB Double

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

coloringFromArray :: A.Array (Int, Int) (Complex Double) -> ColorMap -> Coloring
coloringFromArray arr cmap (i, j) = cmap (Just $ arr A.! (i, j))


coloringFromFunc :: Func -> (Int, Int) -> (Double, Double) -> (Double, Double) 
                 -> ColorMap -> Coloring
coloringFromFunc func (w, h) (xlwr, xupr) (ylwr, yupr) cmap (i, j) = cmap (func z)
    where
        (i', j') = increments (w, h) (xlwr, xupr) (ylwr, yupr) (i, j)
        z = i' :+ j' 


myImage :: Coloring -> (Int, Int) -> Image VU RGB Double
myImage coloring (w, h) = makeImageR VU (w, h) coloring


saveImage :: Func -> (Int, Int) -> (Double, Double) -> (Double, Double) 
          -> ColorMap -> FilePath -> IO ()
saveImage func (w, h) (xlwr, xupr) (ylwr, yupr) cmap file = 
    writeImage file
               myImage (coloringFromFunc func (w, h) (xlwr, xupr) (ylwr, yupr) cmap) (h, w)


saveImage' :: A.Array (Int, Int) (Complex Double) -> (Int, Int) 
              -> ColorMap -> FilePath -> IO ()
saveImage' arr (w, h) cmap file = 
    writeImage file
               myImage coloringFromArray arr cmap (h, w)
