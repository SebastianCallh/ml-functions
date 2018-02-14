module ML.Functions.CostSpec where

import qualified Data.Vector.Storable as V

import           Data.Function        (on)
import           Data.Monoid          (Sum (..))
import           Debug.Trace
import           ML.Functions.Cost    (gini, mse)
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "mse" $
    it "is correct for an example" $ property $ do
      (x, y) <- unzip <$> (listOf1 arbitrary :: Gen [(Double, Double)])
      return $ (mse `on`) V.fromList x y == listMse x y


listMse
  :: (Fractional a, Eq a)
  => [a]
  -> [a]
  -> a
listMse x y = squareSum / nElems
  where
    squareSum = sum (zipWith (\a b -> (a - b)^2) x y)
    nElems    = fromIntegral (length x)
