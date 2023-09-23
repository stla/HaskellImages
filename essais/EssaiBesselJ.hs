import Data.Complex ( Complex(..) )
import SaveImage (saveImage, myImage)
import Math.BesselJ (besselJ, BesselResult(..))
import ColorMaps (colorMap1, colorMap2, colorMap3, colorMap4, colorMap5)
import Graphics.Image hiding (magnitude)
import System.IO.Unsafe (unsafePerformIO)

bess :: Complex Double -> Maybe (Complex Double)
bess z = Just $ _result $ unsafePerformIO $ besselJ (3 :+ (-3)) z 1e-3 100

myimage :: Image VU RGB Double
myimage = myImage bess (1024, 1024) (-10, 10) (-10, 10) colorMap1

saveMyImage :: IO ()
saveMyImage = writeImage "images/BesselJ_nu3-3i.png" myimage

myfilteredImage :: Image VU RGB Double
myfilteredImage = sobelOperator myimage

saveMyFilteredImage :: IO ()
saveMyFilteredImage = writeImage "images/BesselJ_sobel.png" myfilteredImage
