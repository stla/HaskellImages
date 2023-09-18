import Data.Complex ( Complex(..) )
import SaveImage (saveImage, myImage)
import Math.Weierstrass (weierstrassP, ellipticInvariants)
import ColorMaps (colorMap1, colorMap2, colorMap3, colorMap4)
import Graphics.Image hiding (magnitude)

g2g3 :: (Complex Double, Complex Double)
g2g3 = ellipticInvariants (0.5 :+ 0.0) (0.0 :+ 0.5)

g2, g3 :: Complex Double
g2 = fst g2g3
g3 = snd g2g3

wp :: Complex Double -> Maybe (Complex Double)
wp z = Just $ weierstrassP z g2 g3

myimage :: Image VU RGB Double
myimage = myImage wp (512, 512) (-1, 1) (-1, 1) colorMap1

saveMyImage :: IO ()
saveMyImage = writeImage "images/wp.png" myimage

myfilteredImage :: Image VU RGB Double
myfilteredImage = sobelOperator myimage

saveMyFilteredImage :: IO ()
saveMyFilteredImage = writeImage "images/wp_sobel.png" myfilteredImage
