import Data.Complex ( Complex(..) )
import SaveImage (saveImage, myImage)
import Math.Weierstrass (weierstrassP, ellipticInvariants)
import ColorMaps (colorMap1, colorMap2, colorMap3, colorMap4, colorMap5)
import Graphics.Image hiding (magnitude)
import WriteRGB (writeToFileRGB)


wp :: Complex Double -> Maybe (Complex Double)
wp z = Just $ weierstrassP z (0.5 :+ 0.0) (0.0 :+ 0.5)

myimage :: Image VU RGB Double
myimage = myImage wp (512, 512) (-1, 1) (-1, 1) colorMap1

saveMyImage :: IO ()
saveMyImage = writeImage "images/wp.png" myimage

myfilteredImage :: Image VU RGB Double
myfilteredImage = sobelOperator myimage

saveMyFilteredImage :: IO ()
saveMyFilteredImage = writeImage "images/wp_sobel.png" myfilteredImage

lapimg :: Image VU RGB Double
lapimg = applyFilter (laplacianFilter Edge) myimage

saveMyFilteredImage2 :: IO ()
saveMyFilteredImage2 = writeImage "images/wp_Laplacian.png" lapimg

myimage2 :: Image VU RGB Double
myimage2 = myImage wp (512, 512) (-1, 1) (-1, 1) colorMap5
