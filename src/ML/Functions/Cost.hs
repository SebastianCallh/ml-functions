{-|
Module      : ML.Functions.Cost
Description : This module contains common cost function for machine learning models.
Copyright   : (c) Sebastian Callh, 2018
License     : GPL-3
Maintainer  : sebastian.callh@gmail.com
Stability   : experimental
Portability : POSIX

This module contains common cost function for machine learning models.
-}
module ML.Functions.Cost
  ( mse
  , gini
  ) where

import qualified Data.Map             as M
import qualified Data.Vector.Storable as V

import           Data.Vector.Storable (Vector)
import           ML.Functions.Util    (mean)

-- |Mean squared error of two real valued vectors.
--  The two vectors have to have the same size.
mse
  :: (Fractional p, V.Storable p)
  => Vector p -> Vector p
  -> p
mse x y
  | V.length y /= V.length y = error "MSE only defined for vectors of same size."
  | otherwise = mean $ V.zipWith (\a b -> (a - b)^2) x y

-- |Gini impurity as often used in decision trees.
gini
  :: (Ord a, V.Storable a, Fractional b)
  => Vector a
  -> b
gini x = 1.0 - M.foldr' (\n acc -> acc + (n / nOutcomes)^2) 0.0 outcomes
  where
    outcomes  = V.foldr put M.empty x
    put n     = M.insertWith (+) n 1
    nOutcomes = fromIntegral $ V.length x
