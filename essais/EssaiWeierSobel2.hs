import Data.Complex ( Complex(..) )
import SaveImage (saveImage, myImage)
import Math.Weierstrass (weierstrassP, ellipticInvariants)
import ColorMaps (colorMap1, colorMap2, colorMap3, colorMap4, colorMap5)
import Graphics.Image hiding (magnitude)

g2g3 :: (Complex Double, Complex Double)
g2g3 = ellipticInvariants (0.501 :+ 0.0) (0.5 :+ 0.5)

g2, g3 :: Complex Double
g2 = fst g2g3
g3 = snd g2g3

wp :: Complex Double -> Maybe (Complex Double)
wp z = Just $ weierstrassP z g2 g3

myimage :: Image VU RGB Double
myimage = myImage wp (512, 512) (-2, 2) (-2, 2) colorMap2

saveMyImage :: IO ()
saveMyImage = writeImage "images/wp2.png" myimage

myfilteredImage :: Image VU RGB Double
myfilteredImage = sobelOperator myimage

saveMyFilteredImage :: IO ()
saveMyFilteredImage = writeImage "images/wp2_sobel.png" myfilteredImage

