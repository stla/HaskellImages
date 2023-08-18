module ColorMap
    (colorMap)
    where
import Data.Complex ( Complex(..), realPart, imagPart, magnitude, phase )

-- :{
-- fracRem :: (RealFrac a, Integral b) => a -> b -> a
-- fracRem x y = x - fromIntegral (y * (truncate x `quot` y))
-- :}

modulo :: Double -> Int -> Double
modulo a p = 
    let p' = fromIntegral p
    in
    if a > 0 
        then a - fromIntegral(p * floor(a/p'))  
        else a - fromIntegral(p * ceiling(a/p'))

colorMap :: Complex Double -> (Double, Double, Double)
colorMap z = 
    let x = realPart z
    in
    let y = imagPart z
    in
    let a = phase z
    in 
    let r = modulo (magnitude z) 1
    in
    let g = abs(modulo (2*a) 1)
    in
    let b = abs(modulo (x*y) 1)
    in
    (
        (1.0 - cos(r-0.5))*8.0, 
        (1.0 - cos(g-0.5))*8.0, 
        (1.0 - cos(b-0.5))*8.0
    )
