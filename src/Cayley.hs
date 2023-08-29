module Cayley 
  where
import ColorMaps
import SaveImage
import Data.Complex ( Complex(..), magnitude )

cayley :: Func
cayley z = 
    if magnitude z > 0.9999 
        then Nothing
        else Just $ im + (2*im*z) / (im - z)
    where
        im = 0 :+ 1

f :: Complex Double -> Complex Double
f z = 1728 * (z * (z**10 + 11 * z**5 - 1))**5 / 
    (-(z**20 + 1) + 228 * (z**15 - z**5) - 494 * z**10)**3

g :: Func
g z = Just $ f z

save :: ColorMap -> IO ()
save cmap = saveImage g (512, 512) (-5, 5) (-5, 5) cmap "images/aaa.png"
