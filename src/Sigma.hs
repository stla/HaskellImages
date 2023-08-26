module Sigma
    (save1, save2, save3, save4) 
    where
import Data.Complex ( Complex(..) )
import ColorFun (saveImage, saveImage2, saveImage3, saveImage4)
import Math.Weierstrass (weierstrassSigma, ellipticInvariants)

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

complexMap :: Complex Double -> Complex Double
complexMap z = weierstrassSigma z g2 g3

save1 :: IO ()
save1 = saveImage complexMap (width, height) (xlimitLwr, xlimitUpr) (ylimitLwr, ylimitUpr) "Sigma_cm1.png"

save2 :: IO ()
save2 = saveImage2 complexMap (width, height) (xlimitLwr, xlimitUpr) (ylimitLwr, ylimitUpr) "Sigma_cm2.png"

save3 :: Double -> Double -> IO ()
save3 s n = saveImage3 complexMap s n (width, height) (xlimitLwr, xlimitUpr) (ylimitLwr, ylimitUpr) "Sigma_cm3.png"

save4 :: IO ()
save4 = saveImage4 complexMap (width, height) (xlimitLwr, xlimitUpr) (ylimitLwr, ylimitUpr) "Sigma_cm4.png"
