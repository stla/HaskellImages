{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}
module Klein
    (save1, save2, save3, save4)
    where
import SaveImage (saveImage)
import Data.Complex ( Complex(..), magnitude, imagPart )
import Math.Eisenstein (kleinJ)
import ColorMaps (colorMap1, colorMap2, colorMap3, colorMap4)


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

kleinMap :: Complex Double -> Maybe (Complex Double)
kleinMap z = if magnitude z > 0.95
            then Nothing
            else Just (if imagPart z < 0
                then kleinJ (-1 / psi z) / 1728
                else kleinJ (psi z) / 1728)

kleinMapInv :: Complex Double -> Maybe (Complex Double)
kleinMapInv z = maybe k (fmap (1 /)) (Just k)
    where
        k = kleinMap z


save1 :: IO ()
save1 = saveImage kleinMapInv (width, height) (xlimitLwr, xlimitUpr) (ylimitLwr, ylimitUpr) 
        colorMap1 "Klein_cm1.png"

save2 :: IO ()
save2 = saveImage kleinMap (width, height) (xlimitLwr, xlimitUpr) (ylimitLwr, ylimitUpr) 
        colorMap2 "Klein_cm2.png"

save3 :: Double -> Double -> IO ()
save3 s n = saveImage kleinMap (width, height) (xlimitLwr, xlimitUpr) (ylimitLwr, ylimitUpr) 
            (colorMap3 s n) "Klein_cm3.png"

save4 :: IO ()
save4 = saveImage kleinMap (width, height) (xlimitLwr, xlimitUpr) (ylimitLwr, ylimitUpr) 
        colorMap4 "Klein_cm4.png"
