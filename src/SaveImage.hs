module SaveImage
    ( saveImage
    , saveImage'
    , myImage
    , Func )
   where
import Data.Complex ( Complex(..) )
import Graphics.Image
    ( makeImageR, writeImage, RGB, Image, Pixel, VU(..) )
import qualified Data.Array as A (Array, (!), bounds)

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

myImage :: Func -> (Int, Int) -> (Double, Double) -> (Double, Double) 
          -> ColorMap -> Image VU RGB Double
myImage func (w, h) (xlwr, xupr) (ylwr, yupr) cmap = makeImageR VU (w, h) coloring
    where
        coloring = coloringFromFunc func (w, h) (xlwr, xupr) (ylwr, yupr) cmap

myImage' :: A.Array (Int, Int) (Complex Double) -> ColorMap -> Image VU RGB Double
myImage' arr cmap = makeImageR VU (w, h) coloring
    where
        coloring = coloringFromArray arr cmap
        (lwr, upr) = A.bounds arr
        w = fst upr - fst lwr
        h = snd upr - snd lwr


saveImage :: Func -> (Int, Int) -> (Double, Double) -> (Double, Double) 
          -> ColorMap -> FilePath -> IO ()
saveImage func (w, h) (xlwr, xupr) (ylwr, yupr) cmap file = 
    writeImage file
               (myImage func (w, h) (xlwr, xupr) (ylwr, yupr) cmap) 


saveImage' :: A.Array (Int, Int) (Complex Double) 
              -> ColorMap -> FilePath -> IO ()
saveImage' arr cmap file = 
    writeImage file (myImage' arr cmap) 
