import Data.Complex ( Complex(..), magnitude )
import SaveImage (saveImage, myImage)
import Math.JacobiTheta (jtheta4')
import ColorMaps (colorMap1, colorMap2, colorMap3, colorMap4, colorMap5)
import Graphics.Image hiding (magnitude)

rogersRamanujan :: Complex Double -> Complex Double
rogersRamanujan tau = 
  tan(0.5 * x)**0.2 * tan(0.5 * (pi/2 - x))**0.4
  where 
    j1 = jtheta4' 0 tau
    j5 = jtheta4' 0 (5*tau)
    x = atan(0.5 - 0.5 * j1 * j1 / (j5 * j5))

psi :: Complex Double -> Complex Double
psi z = im + 2*im*z / (im - z)
  where
    im = 0 :+ 1

func :: Complex Double -> Maybe (Complex Double)
func z = if magnitude z > 0.98
            then Nothing
            else Just $ rogersRamanujan (psi z) 

myimage :: Image VU RGB Double
myimage = myImage func (512, 512) (-1, 1) (-1, 1) colorMap2

saveMyImage :: IO ()
saveMyImage = writeImage "images/RogRam.png" myimage

myfilteredImage :: Image VU RGB Double
myfilteredImage = sobelOperator myimage

saveMyFilteredImage :: IO ()
saveMyFilteredImage = writeImage "images/RogRam_sobel.png" myfilteredImage

