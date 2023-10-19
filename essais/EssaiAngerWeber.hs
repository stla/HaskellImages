import Data.Complex ( Complex(..) )
import SaveImage (saveImage, myImage)
import Math.AngerWeber (angerWeber, AngerWeberResult(..))
import ColorMaps (colorMap1, colorMap2, colorMap3, colorMap4, colorMap5)
import Graphics.Image hiding (magnitude)
import System.IO.Unsafe (unsafePerformIO)

aw :: Complex Double -> Maybe (Complex Double)
aw z = Just $ _result $ unsafePerformIO $ angerWeber (0 :+ 3) z 1e-3 100

myimage :: Image VU RGB Double
myimage = myImage aw (256, 256) (-40, 0) (-20, 20) colorMap1

saveMyImage :: IO ()
saveMyImage = writeImage "images/AngerWeber2.png" myimage

myfilteredImage :: Image VU RGB Double
myfilteredImage = sobelOperator myimage

saveMyFilteredImage :: IO ()
saveMyFilteredImage = writeImage "images/AngerWeber2_sobel.png" myfilteredImage
