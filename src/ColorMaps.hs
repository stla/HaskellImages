module ColorMaps
    ( colorMap1, colorMap2, colorMap3 )
    where
import Data.Complex ( Complex(..), realPart, imagPart, magnitude, phase )
import Graphics.Image.Interface ()
import Graphics.Image
    ( RGB, Pixel(PixelRGB), Pixel(PixelHSI), toPixelRGB )
import Data.Colour.RGBSpace ( channelRed, channelGreen, channelBlue )
import Data.Colour.RGBSpace.HSL ( hsl )
import Data.Colour.RGBSpace.HSV ( hsv )
import GHC.Float ( log1p )
import Data.Maybe ( isNothing, fromJust )

colorMap1 :: Maybe (Complex Double) -> Pixel RGB Double
colorMap1 z0 = 
    if isNothing z0 
        then PixelRGB 1 1 1
        else 
        let z = fromJust z0
            r = f (magnitude z)
            g = f (2 * phase z)
            b = f (realPart z * imagPart z)
        in
            PixelRGB r g b
        where
            f x = (1.0 - cos(fractpart x - 0.5)) * 8.0
            fractpart x = if x > 0 
                then x - fromIntegral (floor x)
                else x - fromIntegral (ceiling x) 


colorMap2 :: Maybe (Complex Double) -> Pixel RGB Double
colorMap2 z0 =
    if isNothing z0 
        then PixelRGB 1 1 1
        else
        let z = fromJust z0
            a = phase z
            arg = if a < 0 then a + pi else a
            h = min (arg/2/pi) 1
            w = 2 * pi * log1p(abs arg)
            s = sqrt(( 1.0 + sin w ) / 2.0)
            i = ( 1.0 + cos w ) / 2.0
        in 
            toPixelRGB (PixelHSI h s i)


colorMap3 :: Double -> Double -> Maybe (Complex Double) -> Pixel RGB Double
colorMap3 s r z0 =
    if isNothing z0
        then PixelRGB 1 1 1
        else 
        let z = fromJust z0
            arg = phase z * 180 / pi
            h = if arg < 0
                then arg + 360
                else arg
            ph = perFract h (360/r) 216 360 / 360
            plogm = perFract (log1p (magnitude z)) (2 * pi / r) 0.6 1.0
            l = ph * plogm
            color = hsl h s l
            r = channelRed color
            g = channelGreen color
            b = channelBlue color
        in
            PixelRGB r g b
        where
            fromInt :: Int -> Double
            fromInt = fromIntegral
            perFract :: Double -> Double -> Double -> Double -> Double
            perFract x t a b = 
                let xot = x / t
                in 
                a + (b - a) * (xot - fromInt (floor xot))
