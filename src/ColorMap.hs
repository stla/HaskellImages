module ColorMap
    (colorMap, colorMap2)
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


colorMap2 :: Complex Double -> (Double, Double, Double)
colorMap2 z =
    let a = phase z
    in 
    let arg = if a < 0 
        then a + pi 
        else a
    in
    let h = min (arg/2/pi) 0.9999999
    in 
    let w = 2 * pi * log(1 + abs arg)
    in 
    let s = sqrt((1.0 + sin w ) / 2.0)
    in 
    let l = (1.0 + cos w ) / 2.0
    in 
    (h, s, l)

