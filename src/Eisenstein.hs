module Eisenstein
    (myImage
    , funColor
    , saveImage
    ) where
import Data.Complex ( Complex(..), magnitude, realPart, imagPart)
import qualified Data.Complex as DC
import Graphics.Image
    ( makeImageR, writeImage, RGB, Image, Pixel(PixelRGB), Pixel(PixelHSI), VU(..), toPixelRGB, ToRGB (toPixelRGB) )
import ColorMap (colorMap, colorMap2)
import Math.Eisenstein (eisensteinE6)

import Control.Exception

radius :: Double
radius = 0.9

width :: Int 
width = 512
height :: Int 
height = 512
subdivisions :: Double
subdivisions = 1024

width' :: Double
width' = fromIntegral width
height' :: Double
height' = fromIntegral height

-- data AddRGB
--   = Couldn't
--   | CommentFailed
--   deriving (Show)

-- instance Exception AddRGB

-- add :: eisensteinE6 -> IO (q)
-- add q = do
--   code <- tryAdd q
--   case code of
--     (200,val) -> eisensteinE6 q
--     (_,err)   -> throw (error err)

-- data MyRGB = MyRGB { _r :: Double, _g :: Double, _b :: Double }

-- -- picture  = Exc.catch (print $ eisensteinE6) handler
-- --     where
-- --         handler :: Exc.ErrorCall -> IO ()
-- --         handler _ = putStrLn $ "You divided by 0!"


-- checkQtry :: Complex Double -> IO(MyRGB)
-- checkQtry q
--   | magnitude q >= 1 = 
--     MyRGB 0 0 0
--   | imagPart q == 0 && realPart q <= 0 = 
--     MyRGB 0 0 0
--   | otherwise = let (r, g, b) = colorMap (q) in MyRGB r g b

-- colorFun :: (Int, Int) -> Pixel RGB Double
-- colorFun (i, j) = 
--     let i' = width' * fromIntegral i * 2 * subdivisions
--     in
--     let j' = height' * fromIntegral j * 2 * subdivisions
--     in 
--     let z = i' :+ j' 
--     in 
--     let MyRGB r g b = CheckQtry 
--     in
--     PixelRGB r g b

eisenstein :: Int-> Int -> Pixel RGB Double
eisenstein i j = 
    let i = frac 0 / 512
        j = frac 0 / 512 
        z = i :+ j
        modulus = magnitude z
        valid = modulus < 1 && realPart z >= 0 
        case valid of
            True -> Pixel (colormap (eisenstein z))
            False -> Pixel (0, 0, 0)


myImage :: ((Int, Int) -> Pixel RGB Double) -> (Int, Int) -> Image VU RGB Double
myImage thefun (m, n) = makeImageR VU (m, n) thefun

funColor :: Image VU RGB Double
funColor = myImage colorFun (width, height)

saveImage :: FilePath -> IO ()
saveImage file = writeImage ("images/" ++ file) funColor
