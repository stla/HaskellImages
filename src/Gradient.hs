module Gradient
    (colorFun
    , myImage
    , gradientColor
    , saveImage
    ) where
import Graphics.Image
    ( makeImageR, writeImage, RGB, Image, Pixel(PixelRGB), VU(..) )


colorFun :: (Int, Int) -> Pixel RGB Double
colorFun (i, j) =
  PixelRGB (fromIntegral i) (fromIntegral j) (fromIntegral (i + j)) / 400

myImage :: ((Int, Int) -> Pixel RGB Double) -> (Int, Int) -> Image VU RGB Double
myImage thefun (m, n) = makeImageR VU (m, n) thefun

gradientColor :: Image VU RGB Double
gradientColor = myImage colorFun (400, 400)

saveImage :: IO ()
saveImage = writeImage "images/mygradient.png" gradientColor
