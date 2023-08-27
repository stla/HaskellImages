{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module BesselCSV
  where
import qualified Data.Vector as V (Vector, (!), length)
-- from base
import GHC.Generics
import System.IO
import System.Exit (exitFailure)
-- from bytestring
import Data.ByteString (ByteString, hGetSome, empty)
import qualified Data.ByteString.Lazy as BL
import Control.Monad ( mzero )
-- from cassava
import Data.Complex ( Complex((:+)) )
import Data.Csv ( (.!), FromRecord(..), decode, HasHeader(..), ToRecord(..), toField, record )
import qualified Data.Array as A (Array, array)
import Data.Either (fromRight)

data Cplx = Cplx { re :: !Double, im :: !Double }

cplxToComplex :: Cplx -> Complex Double
cplxToComplex cplx = re cplx :+ im cplx

instance FromRecord Cplx where
    parseRecord v
        | V.length v == 2 = Cplx <$> (v .! 0) <*> (v .! 1)
        | otherwise     = mzero
instance ToRecord Cplx where
    toRecord (Cplx re' im') = record [
        toField re', toField im']

besselColumns :: IO (Either String (V.Vector Cplx))
besselColumns = do
    csvData <- BL.readFile "Bessel.csv"
    return $ decode HasHeader csvData 
        
besselArray :: V.Vector Cplx -> A.Array (Int, Int) (Complex Double)
besselArray vcplx = 
    let assocList :: [((Int, Int), Complex Double)]
        assocList = 
         [((i, j), cplxToComplex (vcplx V.! (i*511 + j))) | i <- [0 .. 511], j <- [0 .. 511]]
    in
    A.array ((0, 0), (511, 511)) assocList

