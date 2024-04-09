
import Molecule
import Molecules.Methane
import GenerativeMolecule
import Constants
import Control.Monad
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Traced
import Control.Monad.Bayes.Sampler
import Data.List (sort)
import Numeric.Log( Log( Exp ), ln )


-- -- Define a simple model
simpleModel :: MonadInfer m => Double -> m Double
simpleModel threshold = do
  sample <- normal 0 1
  score (if ((sample > threshold) && (sample < threshold + 0.1)) then 100 else 0)
  return sample


-- simpleModelTraced :: MonadInfer m => Double -> Traced (Sequential m) Double
-- simpleModelTraced threshold = do
--   sample <- normal 0 1
--   score (if ((sample > threshold) && (sample < threshold + 0.1)) then 100 else 0)
--   return sample

-- -- Assuming simpleModel is compatible with MonadInfer
-- simpleModelTraced :: MonadInfer m => Double -> Traced m Double
-- simpleModelTraced dbl = simpleModel dbl -- if simpleModel uses MonadInfer, it might be directly compatible

-- main :: IO ()
-- main = do
--   let steps = 100000     -- Number of steps for MH
--       threshold = 3.0  -- Threshold parameter for your model
  
--   -- Execute the MH algorithm and print the results
--   results <- sampleIO $ marginal $ mh steps (simpleModelTraced threshold)
--   print "results"


-- --runMHs :: Int -> Double -> Weighted SamplerIO [Double]
-- --runMHs steps threshold = mh steps (simpleModel threshold)


