{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wwarn #-}

module Data.Distribution
  ( ToRealFrac(..)
  , Distribution(..)
  , computeDistribution
  , zeroDistribution
  , PercSpec(..)
  , renderPercSpec
  , Percentile(..)
  , pctFrac
  , stdPercentiles
  -- Aux
  , spans
  ) where

import           Prelude (String, (!!), fail, head)
import           Cardano.Prelude hiding (head)

import           Control.Arrow
import           Data.Aeson (ToJSON(..))
import qualified Data.Foldable as F
import           Data.List (span, transpose)
import           Data.Vector (Vector)
import qualified Data.Vector as Vec
import           Text.Printf (PrintfArg, printf)

data Distribution a b =
  Distribution
  { dAverage      :: a
  , dCount        :: Int
  , dPercentiles  :: [Percentile a b]
  }
  deriving (Generic, Show)

instance (ToJSON a, ToJSON b) => ToJSON (Distribution a b)

newtype PercSpec a = Perc { psFrac :: a } deriving (Generic, Show)

renderPercSpec :: PrintfArg a => Int -> PercSpec a -> String
renderPercSpec width = \case
  Perc x    -> printf ("%0."<>show (width-2)<>"f") x

data Percentile a b =
  Percentile
  { pctSpec        :: !(PercSpec a)
  , pctSample      :: !b
  }
  deriving (Generic, Show)

pctFrac :: Percentile a b -> a
pctFrac = psFrac . pctSpec

stdPercentiles :: [PercSpec Float]
stdPercentiles =
  [ Perc 0.01, Perc 0.05
  , Perc 0.1, Perc 0.2, Perc 0.3, Perc 0.4
  , Perc 0.5, Perc 0.6
  , Perc 0.7, Perc 0.75
  , Perc 0.8, Perc 0.85, Perc 0.875
  , Perc 0.9, Perc 0.925, Perc 0.95, Perc 0.97, Perc 0.98, Perc 0.99
  , Perc 0.995, Perc 0.997, Perc 0.998, Perc 0.999
  , Perc 0.9995, Perc 0.9997, Perc 0.9998, Perc 0.9999
  ]

instance (ToJSON a) => ToJSON (PercSpec a)
instance (ToJSON a, ToJSON b) => ToJSON (Percentile a b)

zeroDistribution :: Num a => Distribution a b
zeroDistribution =
  Distribution
  { dAverage     = 0
  , dCount       = 0
  , dPercentiles = mempty
  }

countSeq :: Eq a => a -> [a] -> Int
countSeq x = foldl' (\n e -> if e == x then n + 1 else n) 0

-- | For a list of distributions, compute a distribution of averages and standard deviations.
computeDistributionStats ::
    forall a v
  . (RealFrac a, Real v, Fractional v, ToRealFrac v a)
  => [Distribution a v]
  -> Either String (Distribution a v, Distribution a v)
computeDistributionStats xs = do
  let distPcts    = dPercentiles <$> xs
      pctDistVals = transpose distPcts
  unless (all (pctLen ==) (length <$> distPcts)) $
    Left ("Distributions with different percentile counts: " <> show (length <$> distPcts))
  pure undefined
 where
   nPcts  = length xs
   pctLen = length . dPercentiles $ head xs

   pctsAvg :: [Percentile a v] -> Percentile a v
   pctsAvg xs = Percentile (pctSpec $ head xs) (sum (pctSample <$> xs) / fromIntegral nPcts)

computeDistribution :: (RealFrac a, Real v, ToRealFrac v a) => [PercSpec a] -> [v] -> Distribution a v
computeDistribution percentiles (sort -> sorted) =
  Distribution
  { dAverage     = toRealFrac (F.sum sorted) / fromIntegral (size `max` 1)
  , dCount       = size
  , dPercentiles =
    (Percentile     (Perc 0)   mini:) .
    (<> [Percentile (Perc 1.0) maxi]) $
    percentiles <&>
      \spec ->
        let (sampleIndex :: Int, sample) =
              if size == 0
              then (0, fromInteger 0)
              else floor (fromIntegral (size - 1) * psFrac spec) &
                   ((\x->x) &&& (sorted !!))
        in Percentile spec sample
  }
  where size = length sorted
        (,) mini maxi =
          if size == 0
          then (0, fromInteger 0)
          else (sorted !! 0, sorted !! (size - 1))

class RealFrac b => ToRealFrac a b where
  toRealFrac :: a -> b

instance RealFrac b => ToRealFrac Int b where
  toRealFrac = fromIntegral

instance {-# OVERLAPPABLE #-} (RealFrac b, Real a) => ToRealFrac a b where
  toRealFrac = realToFrac

spans :: forall a. (a -> Bool) -> [a] -> [Vector a]
spans f = go []
 where
   go :: [Vector a] -> [a] -> [Vector a]
   go acc [] = reverse acc
   go acc xs =
     case span f $ dropWhile (not . f) xs of
       ([], rest) -> go acc rest
       (ac, rest) ->
         go (Vec.fromList ac:acc) rest
