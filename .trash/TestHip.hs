module TestHip
    (fun, test, test2, myimage, grad_color, writeImage) where
import Graphics.Image
import Graphics.Image.IO 
import Graphics.Image.Interface 

someFunc :: IO ()
someFunc = putStrLn "someFunc"



--test :: (Int, Int) -> Image arr cs e
test (m, n) = 
    makeImageR VU (200, 200) (\(i, j) -> PixelRGB  (fromIntegral i) (fromIntegral j) (fromIntegral (i + j))) / 400 

test2 = (\(i, j) -> PixelRGB i j  ((i + j) / 400)) 

--fun :: (Int, Int) -> Pixel RGB Word8
--fun :: (hip-1.5.6.0:Graphics.Image.Interface.Elevator.Elevator e,
-- Fractional e) =>
-- (a, b) -> Image VU RGB e

fun :: (Int, Int) -> Pixel RGB Double
fun (i, j) =
  PixelRGB (fromIntegral i) (fromIntegral j) (fromIntegral (i + j)) / 400

myimage :: ((Int, Int) -> Pixel RGB Double) -> (Int, Int) -> Image VU RGB Double
myimage thefun (m, n) = makeImageR VU (m, n) thefun


grad_color = myimage fun (400, 400)


--w = writeImage "mages/grad_color400.png" grad_color 