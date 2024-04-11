module Extra where 
import LazyPPL

uniformBounded :: Double -> Double -> Prob Double
uniformBounded lower upper = do
  x <- uniform
  return $ (upper - lower) * x + lower

uniformDiscrete :: Int -> Prob Int
uniformDiscrete n =
  do
    let upper = fromIntegral n
    r <- uniformBounded 0 upper
    return $ floor r

uniformD :: [a] -> Prob a
uniformD xs = let l = length xs
              in  do i <- uniformDiscrete l
                     return $ xs !! i

condition :: Bool -> Meas ()
condition True = score 1
condition False = score 0
