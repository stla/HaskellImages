module ColorMap
    (colorMap, colorMap2, colorMap3)
    where
import Data.Complex ( Complex(..), realPart, imagPart, magnitude, phase )
import Graphics.Image.Interface ()
import Data.Colour.RGBSpace (RGB ( .. ))
import Data.Colour.RGBSpace.HSL ( hsl )
import GHC.Float (log1p)

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

-- colorMap3 :: Complex Double -> (Double, Double, Double)
-- colorMap3 z = 
--   let (r, g, b) = colorMap z
--   in
--   let
--   i = (r + g + b) / 3
--   in
--   let minmin = min (min r g) b
--   in
--   let
--   s = if i == 0 then 0 else 1 - minmin / i
--   in 
--   let rad = sqrt(r*r+g*g+b*b-r*g-r*b-g*b)
--   in 
--   let acs = acos(r-g/2-b/2)
--   in
--   let h = if g >= b then acs*rad else 2*pi-acs*rad
--   in 
--     (
--         h, s, i
--     )

-- -- data MyRGB = MyRGB { _r :: Double, _g :: Double, _b :: Double }

-- hsi2RGB :: (Double, Double, Double) -> (Double, Double, Double) 
-- hsi2RGB (h, s, i) | h < 0 = error "`h` must be nonnegative."
--                   | h < 2*pi/3 = let !r = getFirst h (pi/3 - h)
--                                      !b = second
--                                      !g = getThird b r
--                                  in (r, g, b)
--                   | h < 4*pi/3 = let !g = getFirst (h - 2*pi/3) (h + pi)
--                                      !r = second
--                                      !b = getThird r g
--                                  in (r, g, b)
--                   | h <= 2*pi = let !b = getFirst (h - 4*pi/3) (2*pi - pi/3 - h)
--                                     !g = second
--                                     !r = getThird g b
--                                 in (r, g, b)
--                  | otherwise    = error "`h` must be less than 2pi."
--     where   
--         !is = i*s
--         !second = i - is
--         getFirst !a !b = i + is * cos a / cos b
--         getThird !v1 !v2 = i + 2*is + v1 - v2



perFract :: Double -> Double -> Double -> Double -> Double
perFract x t a b = 
    a + (b -a) * ((x / t) - fromIntegral (floor(x / t)))


colorMap3' :: Complex Double -> Double -> Double -> RGB Double
colorMap3' z s r = 
    let arg = phase z * 57.29577951308232087680 -- (180 / pi) --
        h = if arg < 0
            then arg + 360
            else arg
        ph = perFract h (360/r) 216 360 / 360
        plogm = perFract (log1p (magnitude z)) (2 * pi / r) 0.6 1.0
        l = ph * plogm
    in
    hsl h s l

colorMap3 :: Complex Double -> Double -> Double -> (Double, Double, Double)
colorMap3 z s r = 
    let rgb = colorMap3' z s r
        red = channelRed rgb
        green = channelGreen rgb
        blue = channelBlue rgb
    in (red, green, blue)