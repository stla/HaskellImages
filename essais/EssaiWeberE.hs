import Data.Complex ( Complex(..) )
import SaveImage (saveImage, myImage)
import Math.WeberE (weberE, WeberResult(..))
import ColorMaps (colorMap1, colorMap2, colorMap3, colorMap4, colorMap5)
import Graphics.Image hiding (magnitude)
import System.IO.Unsafe (unsafePerformIO)

web :: Complex Double -> Maybe (Complex Double)
web z = Just $ _result $ unsafePerformIO $ weberE 3 z 1e-3 100

myimage :: Image VU RGB Double
myimage = myImage web (1024, 1024) (-10, 10) (-10, 10) colorMap1

saveMyImage :: IO ()
saveMyImage = writeImage "images/WeberE_nu3.png" myimage

myfilteredImage :: Image VU RGB Double
myfilteredImage = sobelOperator myimage

saveMyFilteredImage :: IO ()
saveMyFilteredImage = writeImage "images/WeberE_sobel.png" myfilteredImage
