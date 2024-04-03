import Control.Monad
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Traced.Basic
import Control.Monad.Bayes.Weighted
import Data.List (sort)
import Numeric.Log( Log( Exp ), ln )


-- Define a simple model
simpleModel :: MonadInfer m => Double -> m Double
simpleModel threshold = do
  sample <- normal 0 1
  score (if ((sample > threshold) && (sample < threshold + 0.1)) then 100 else 0)
  return sample

-- Assuming simpleModel is compatible with MonadInfer
simpleModelTraced :: MonadInfer m => Double -> Traced m Double
simpleModelTraced dbl = simpleModel dbl -- if simpleModel uses MonadInfer, it might be directly compatible

runMH :: MonadInfer m => Int -> Double -> m [Double]
runMH steps threshold = mh steps (simpleModelTraced threshold)


main :: IO ()
main = do
  let steps = 100000     -- Number of steps for MH
      threshold = 3.0  -- Threshold parameter for your model
  
  -- Execute the MH algorithm and print the results
  results <- sampleIOfixed $ runWeighted $ runMH steps threshold
  print results
  print (sum (fst results) / fromIntegral (length (fst results)))


--runMHs :: Int -> Double -> Weighted SamplerIO [Double]
--runMHs steps threshold = mh steps (simpleModel threshold)