{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module WriteRGB (writeToFileRGB) where

-- from base
import GHC.Generics ( Generic )
-- from bytestring
import qualified Data.ByteString.Lazy as BL
-- from cassava
import Data.Csv.Incremental ( encode, encodeRecord )
import Data.Csv (FromRecord, ToRecord)
-- from hip
import Graphics.Image (RGB, Pixel(PixelRGB), Image, VU(..))
import Graphics.Image.Interface (dims, index)

data Value = Value
  { _i :: !Int, _j :: !Int, _value :: !Double } 
  deriving (Show, Eq, Generic)

instance FromRecord Value
instance ToRecord Value

getR :: Pixel RGB Double -> Double
getR px = r 
  where
    PixelRGB r _ _ = px

getRed :: Image VU RGB Double -> [Value]
getRed img = 
    let (w, h) = dims img
    in
        [Value (i+1) (j+1) (getR $ index img (i,j)) | i <- [0 .. w-1], j <- [0 .. h-1]]

getG :: Pixel RGB Double -> Double
getG px = g 
  where
    PixelRGB _ g _ = px

getGreen :: Image VU RGB Double -> [Value]
getGreen img = 
    let (w, h) = dims img
    in
        [Value (i+1) (j+1) (getG $ index img (i,j)) | i <- [0 .. w-1], j <- [0 .. h-1]]

getB :: Pixel RGB Double -> Double
getB px = b 
  where
    PixelRGB _ _ b = px

getBlue :: Image VU RGB Double -> [Value]
getBlue img = 
    let (w, h) = dims img
    in
        [Value (i+1) (j+1) (getB $ index img (i,j)) | i <- [0 .. w-1], j <- [0 .. h-1]]

writeToFileR :: Image VU RGB Double -> IO ()
writeToFileR img = do
  BL.writeFile "R.csv" $ encode $
    foldMap encodeRecord (getRed img)

writeToFileG :: Image VU RGB Double -> IO ()
writeToFileG img = do
  BL.writeFile "G.csv" $ encode $
    foldMap encodeRecord (getGreen img)

writeToFileB :: Image VU RGB Double -> IO ()
writeToFileB img = do
  BL.writeFile "B.csv" $ encode $
    foldMap encodeRecord (getBlue img)

writeToFileRGB :: Image VU RGB Double -> IO ()
writeToFileRGB img = do 
  writeToFileR img
  writeToFileG img
  writeToFileB img