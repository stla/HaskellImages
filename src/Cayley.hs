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