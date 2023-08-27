{-# LANGUAGE OverloadedStrings #-}
module BesselCSV
    ( save1, save2, save3, save4 )
    where
import qualified Data.Vector as V ( Vector, (!), length )
import qualified Data.ByteString.Lazy as BL
import Control.Monad ( mzero )
import Data.Complex ( Complex((:+)) )
import Data.Csv ( (.!), FromRecord(..), decode, HasHeader(..), ToRecord(..), toField, record )
import qualified Data.Array as A ( Array, array )
import Data.Either.Extra ( fromRight' )
import ColorFun (saveImage', saveImage2', saveImage3', saveImage4')

data Cplx = Cplx { re :: !Double, im :: !Double }

cplxToComplex :: Cplx -> Complex Double
cplxToComplex cplx = re cplx :+ im cplx

instance FromRecord Cplx where
    parseRecord v
        | V.length v == 2 = Cplx <$> (v .! 0) <*> (v .! 1)
        | otherwise     = mzero
instance ToRecord Cplx where
    toRecord (Cplx re' im') = 
        record [toField re', toField im']

besselColumns :: IO (Either String (V.Vector Cplx))
besselColumns = do
    csvData <- BL.readFile "Bessel.csv"
    return $ decode HasHeader csvData 
        
besselArray :: V.Vector Cplx -> A.Array (Int, Int) (Complex Double)
besselArray vcplx = 
    let assocList :: [((Int, Int), Complex Double)]
        assocList = 
         [((i, j), cplxToComplex (vcplx V.! (i + j*512))) | i <- [0 .. 511], j <- [0 .. 511]]
    in
    A.array ((0, 0), (511, 511)) assocList

save1 :: IO ()
save1 = do
    eithervcplx <- besselColumns
    let vcplx = fromRight' eithervcplx
        arr = besselArray vcplx
    saveImage' arr (512, 512) "Bessel_cm1.png"

save2 :: IO ()
save2 = do
    eithervcplx <- besselColumns
    let vcplx = fromRight' eithervcplx
        arr = besselArray vcplx
    saveImage2' arr (512, 512) "Bessel_cm2.png"

save3 :: IO ()
save3 = do
    eithervcplx <- besselColumns
    let vcplx = fromRight' eithervcplx
        arr = besselArray vcplx
    saveImage3' arr 0.8 20 (512, 512) "Bessel_cm3.png"

save4 :: IO ()
save4 = do
    eithervcplx <- besselColumns
    let vcplx = fromRight' eithervcplx
        arr = besselArray vcplx
    saveImage4' arr (512, 512) "Bessel_cm4.png"