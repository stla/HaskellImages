module BesselData
  where
import Data.Complex
import BesselDataX
import BesselDataY
import qualified Data.Vector.Unboxed as VU
import qualified Data.Array.Unboxed  as A  (UArray)

besselData :: UArray (Int, Int) (Complex Double)
