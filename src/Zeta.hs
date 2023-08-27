module Zeta
    (save1, save2, save3, save4) 
    where
import Data.Complex ( Complex(..) )
import ColorFun (saveImage, saveImage2, saveImage3, saveImage4)
import Math.Weierstrass (weierstrassZeta, ellipticInvariants)

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
g2g3 = ellipticInvariants (1 :+ 0) (isqr3/2 :+ 0.5)
    where
    isqr3 = 1 / sqrt 3

g2, g3 :: Complex Double
g2 = fst g2g3
g3 = snd g2g3

complexMap :: Complex Double -> Maybe (Complex Double)
complexMap z = Just $ weierstrassZeta z g2 g3

save1 :: IO ()
save1 = saveImage complexMap (width, height) (xlimitLwr, xlimitUpr) (ylimitLwr, ylimitUpr) "Zeta_cm1.png"

save2 :: IO ()
save2 = saveImage2 complexMap (width, height) (xlimitLwr, xlimitUpr) (ylimitLwr, ylimitUpr) "Zeta_cm2.png"

save3 :: Double -> Double -> IO ()
save3 s n = saveImage3 complexMap s n (width, height) (xlimitLwr, xlimitUpr) (ylimitLwr, ylimitUpr) "Zeta_cm3.png"

save4 :: IO ()
save4 = saveImage4 complexMap (width, height) (xlimitLwr, xlimitUpr) (ylimitLwr, ylimitUpr) "Zeta_cm4.png"
