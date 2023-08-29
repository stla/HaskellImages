module Cayley 
  where
import ColorMaps
import SaveImage
import Data.Complex ( Complex(..), magnitude )
import Graphics.Image hiding (magnitude)

cayley :: Func
cayley z = 
    if magnitude z >= 1
        then Nothing
        else Just $ im + (2*im*z) / (im - z)
    where
        im = 0 :+ 1

f :: Complex Double -> Complex Double
f z = 1728 * (z * (z**10 + 11 * z**5 - 1))**5 / 
    (-(z**20 + 1) + 228 * (z**15 - z**5) - 494 * z**10)**3

g :: Func
g z = Just $ f $ f z

savef :: ColorMap -> IO ()
savef cmap = saveImage g (512, 512) (-5, 5) (-5, 5) cmap "images/aaa.png"

save :: ColorMap -> IO ()
save cmap = 
    saveImage cayley (512, 512) (-1, 1) (-1, 1) cmap "images/Cayley.png"

img :: Image VU RGB Double
img = myImage g (512, 512) (-5, 5) (-5, 5) colorMap4

imgX = convolve Edge (fromLists [[-1, 0, 1], [-2, 0, 2], [-1, 0, 1]]) img
imgY = convolve Edge (fromLists [[-1,-2,-1], [ 0, 0, 0], [ 1, 2, 1]]) img
imgXY = normalize $ sqrt (imgX ^ 2 + imgY ^ 2)