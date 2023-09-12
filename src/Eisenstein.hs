module Eisenstein
    (save1, save2, save3, save4)
    where
import SaveImage (saveImage)
import Data.Complex ( Complex(..), magnitude, realPart, imagPart)
import Math.Eisenstein (eisensteinE4)
import ColorMaps (colorMap1, colorMap2, colorMap3, colorMap4)


xlimitLwr, xlimitUpr :: Double
xlimitLwr = 0
xlimitUpr = 1

ylimitLwr, ylimitUpr :: Double
ylimitLwr = 0
ylimitUpr = 1

width, height :: Int
width = 512
height = 512

modulusOK :: Complex Double -> Bool
modulusOK tau = 
    magnitude q < 0.8 && (imagPart q > 0 || realPart q < 0.8) 
    where q = exp((0 :+ 1) * tau * pi * 8)

e4map :: Complex Double -> Maybe (Complex Double)
e4map z = if modulusOK z 
            then Just (eisensteinE4 z)
            else Nothing

save1 :: IO ()
save1 = 
    saveImage e4map (width, height) (xlimitLwr, xlimitUpr) (ylimitLwr, ylimitUpr) 
    colorMap1 "Eisen4_cm1.png"

save2 :: IO ()
save2 = saveImage e4map (width, height) (xlimitLwr, xlimitUpr) (ylimitLwr, ylimitUpr) 
        colorMap2 "Eisen4_cm2.png"

save3 :: Double -> Double -> IO ()
save3 s n = saveImage e4map (width, height) (xlimitLwr, xlimitUpr) (ylimitLwr, ylimitUpr) 
            (colorMap3 s n) "Eisen4_cm3.png"

save4 :: IO ()
save4 = saveImage e4map (width, height) (xlimitLwr, xlimitUpr) (ylimitLwr, ylimitUpr) 
        colorMap4 "Eisen4_cm4.png"
