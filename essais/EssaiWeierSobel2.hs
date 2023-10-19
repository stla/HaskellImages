import Data.Complex ( Complex(..) )
import SaveImage (saveImage, myImage)
import Math.Weierstrass (weierstrassP)
import ColorMaps (colorMap1, colorMap2, colorMap3, colorMap4, colorMap5)
import Graphics.Image hiding (magnitude)

wp :: Complex Double -> Maybe (Complex Double)
wp z = Just $ weierstrassP z (0.5 :+ 0.0) (0.5 :+ 0.5)

myimage :: Image VU RGB Double
myimage = myImage wp (512, 512) (-2, 2) (-2, 2) colorMap2

saveMyImage :: IO ()
saveMyImage = writeImage "images/wp2.png" myimage

myfilteredImage :: Image VU RGB Double
myfilteredImage = sobelOperator myimage

saveMyFilteredImage :: IO ()
saveMyFilteredImage = writeImage "images/wp2_sobel.png" myfilteredImage

