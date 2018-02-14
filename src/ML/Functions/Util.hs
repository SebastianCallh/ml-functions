{-|
Module      : ML.Functions.Cost
Description : This module contains common cost function for machine learning models.
Copyright   : (c) Sebastian Callh, 2018
License     : GPL-3
Maintainer  : sebastian.callh@gmail.com
Stability   : experimental
Portability : POSIX

This module contains common utility functions used in machine learning.
-}
module ML.Functions.Util
  ( mean
  ) where

import qualified Data.Vector.Storable as V


-- |The mean of a vector.
mean
  :: (V.Storable a, Fractional a)
  => V.Vector a
  -> a
mean x = V.sum x / fromIntegral (V.length x)
