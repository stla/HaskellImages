{-# LANGUAGE BangPatterns          #-}

module ColorMap
    (colorMap, colorMap2, colorMap3, getRGB)
    where
import Data.Complex ( Complex(..), realPart, imagPart, magnitude, phase )
import Graphics.Image.Interface ()

-- :{ -- this is the same as the modulo function below
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
        y = imagPart z
        a = phase z
        r = modulo (magnitude z) 1
    in
    let g = abs(modulo (2*a) 1)
    in
    let b = abs(modulo (x*y) 1)
    in
    (
        (1.0 - cos(r - 0.5)) * 8.0, 
        (1.0 - cos(g - 0.5)) * 8.0, 
        (1.0 - cos(b - 0.5)) * 8.0
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
    let i = (1.0 + cos w ) / 2.0
    in 
    (h, s, i)

-- colorMap3 :: Complex c -> (Double, Double, Double)
-- colorMap3 z =
--     PixelRGB r g b
--     where 
--     lu = max (max r g) b
--         where v = lu
--               li = min (min r g) b   
--               l = (lu - li) / 2 
--               a = phase z
--               arg = if a < 0 then a + pi else a
--               h = min (arg/2/pi) 0.9999999
--               w = 2 * pi * log(1 + abs arg)
--               s = sqrt((1.0 + sin w ) / 2.0)
--               i = (1.0 + cos w ) / 2.0
--               hh = (rr + gg + bb) / 3
--                 where 
--                     toPixelHSI (PixelHSI hh s i) = PixelRGB r g b

colorMap3 :: Complex Double -> (Double, Double, Double)
colorMap3 z = 
  let (r, g, b) = colorMap z
  in
  let
  i = (r + g + b) / 3
  in
  let minmin = min (min r g) b
  in
  let
  s = if i == 0 then 0 else 1 - minmin / i
  in 
  let rad = sqrt(r*r+g*g+b*b-r*g-r*b-g*b)
  in 
  let acs = acos(r-g/2-b/2)
  in
  let h = if g >= b then acs*rad else 2*pi-acs*rad
  in 
    (
        h, s, i
    )

data MyRGB = MyRGB { _r :: Double, _g :: Double, _b :: Double }

getRGB :: (Double, Double, Double) -> MyRGB 
getRGB (h, s, i) | h < 0 = error "x"
                 | h < 2*pi/3 = let !r = getFirst h (pi/3 - h)
                                    !b = second
                                    !g = getThird b r
                                in MyRGB r g b
                 | h < 4*pi/3 = let !g = getFirst (h - 2*pi/3) (h + pi)
                                    !r = second
                                    !b = getThird r g
                                 in MyRGB r g b
                 | h <= 2*pi = let !b = getFirst (h - 4*pi/3) (2*pi - pi/3 - h)
                                   !g = second
                                   !r = getThird g b
                               in MyRGB r g b
                 | otherwise    = error "y"
    where   
        !is = i*s
        !second = i - is
        getFirst !a !b = i + is * cos a / cos b
        getThird !v1 !v2 = i + 2*is + v1 - v2
 