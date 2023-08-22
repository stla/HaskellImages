module Palette
  (colorRamp, colorRamp1)
  where
import qualified Data.Vector.Unboxed          as V
import           Numeric.Tools.Interpolation  (at, cubicSpline, tabulate)
import           Numeric.Tools.Mesh           (uniformMesh)
import           Data.Tuple.Extra             (fst3, snd3, thd3)
type Palette = [(Double, Double, Double)]

colorRamp :: Palette -> Double -> (Double, Double, Double) 
colorRamp palette x =
  (tbl_r `at` x, tbl_g `at` x, tbl_b `at` x)
  where
    umesh = uniformMesh (0,1) (length palette)
    r = V.fromList $ map fst3 palette
    g = V.fromList $ map snd3 palette
    b = V.fromList $ map thd3 palette
    tab_r = tabulate umesh r
    tab_g = tabulate umesh g
    tab_b = tabulate umesh b
    tbl_r = cubicSpline tab_r
    tbl_g = cubicSpline tab_g
    tbl_b = cubicSpline tab_b

palette1 :: Palette
palette1 = 
  [
  (1.0000000, 0.0000000, 0.0000000),
  (1.0000000, 0.6470588, 0.0000000),
  (1.0000000, 1.0000000, 0.0000000),
  (0.0000000, 1.0000000, 0.0000000),
  (0.2509804, 0.8784314, 0.8156863),
  (0.0000000, 1.0000000, 1.0000000),
  (0.0000000, 0.0000000, 1.0000000),
  (0.9333333, 0.5098039, 0.9333333),
  (1.0000000, 0.0000000, 1.0000000),
  (1.0000000, 0.0000000, 0.0000000)
  ]

colorRamp1 :: Double -> (Double, Double, Double) 
colorRamp1 = colorRamp palette1
