module KleinFibonacci
    (save1, save2, save3, save4)
    where
import ColorFun (saveImage, saveImage2, saveImage3, saveImage4)
import Data.Complex ( Complex(..), magnitude, imagPart )
import Math.Eisenstein (kleinJ)


xlimitLwr, xlimitUpr :: Double
xlimitLwr = -1
xlimitUpr = 1

ylimitLwr, ylimitUpr :: Double
ylimitLwr = -1
ylimitUpr = 1

width, height :: Int
width = 512
height = 512

psi :: Complex Double -> Complex Double
psi z = im + 2*im*z / (im - z)
  where
    im = 0 :+ 1

fibo :: Complex Double -> Complex Double
fibo z = z / (1 - z - z*z)

kleinMap :: Complex Double -> Maybe (Complex Double)
kleinMap z = if magnitude z > 0.95
            then Nothing
            else Just (if imagPart z < 0
                then fibo (kleinJ (-1 / psi z) / 1728)
                else fibo (kleinJ (psi z) / 1728))


save1 :: IO ()
save1 = saveImage kleinMap (width, height) (xlimitLwr, xlimitUpr) (ylimitLwr, ylimitUpr) "KleinFibo_cm1.png"

save2 :: IO ()
save2 = saveImage2 kleinMap (width, height) (xlimitLwr, xlimitUpr) (ylimitLwr, ylimitUpr) "KleinFibo_cm2.png"

save3 :: Double -> Double -> IO ()
save3 s n = saveImage3 kleinMap s n (width, height) (xlimitLwr, xlimitUpr) (ylimitLwr, ylimitUpr) "KleinFibo_cm3.png"

save4 :: IO ()
save4 = saveImage4 kleinMap (width, height) (xlimitLwr, xlimitUpr) (ylimitLwr, ylimitUpr) "KleinFibo_cm4.png"
