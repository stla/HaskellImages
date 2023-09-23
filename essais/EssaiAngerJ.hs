import Data.Complex ( Complex(..) )
import SaveImage (saveImage, myImage)
import Math.AngerJ (angerJ, AngerResult(..))
import ColorMaps (colorMap1, colorMap2, colorMap3, colorMap4, colorMap5)
import Graphics.Image hiding (magnitude)
import System.IO.Unsafe (unsafePerformIO)

ang :: Complex Double -> Maybe (Complex Double)
ang z = Just $ _result $ unsafePerformIO $ angerJ 3 z 1e-3 100

myimage :: Image VU RGB Double
myimage = myImage ang (1024, 1024) (-10, 10) (-10, 10) colorMap1

saveMyImage :: IO ()
saveMyImage = writeImage "images/AngerJ_nu3.png" myimage

myfilteredImage :: Image VU RGB Double
myfilteredImage = sobelOperator myimage

saveMyFilteredImage :: IO ()
saveMyFilteredImage = writeImage "images/AngerJ_sobel.png" myfilteredImage
