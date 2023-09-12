module Sigma
    (save1, save2, save3, save4) 
    where
import Data.Complex ( Complex(..) )
import SaveImage (saveImage)
import Math.Weierstrass (weierstrassSigma, ellipticInvariants)
import ColorMaps (colorMap1, colorMap2, colorMap3, colorMap4)

xlimitLwr, xlimitUpr :: Double
xlimitLwr = -5
xlimitUpr = 5

ylimitLwr, ylimitUpr :: Double
ylimitLwr = -5
ylimitUpr = 5

width, height :: Int 
width = 512
height = 512

g2g3 :: (Complex Double, Complex Double)
g2g3 = ellipticInvariants (1 :+ 0) (0.25 :+ 1)

g2, g3 :: Complex Double
g2 = fst g2g3
g3 = snd g2g3

complexMap :: Complex Double -> Maybe (Complex Double)
complexMap z = Just $ weierstrassSigma z g2 g3

save1 :: IO ()
save1 = saveImage complexMap (width, height) (xlimitLwr, xlimitUpr) (ylimitLwr, ylimitUpr) 
        colorMap1 "Sigma_cm1.png"

save2 :: IO ()
save2 = saveImage complexMap (width, height) (xlimitLwr, xlimitUpr) (ylimitLwr, ylimitUpr) 
        colorMap2 "Sigma_cm2.png"

save3 :: Double -> Double -> IO ()
save3 s n = saveImage complexMap (width, height) (xlimitLwr, xlimitUpr) (ylimitLwr, ylimitUpr) 
            (colorMap3 s n) "Sigma_cm3.png"

save4 :: IO ()
save4 = saveImage complexMap (width, height) (xlimitLwr, xlimitUpr) (ylimitLwr, ylimitUpr) 
        colorMap4 "Sigma_cm4.png"
