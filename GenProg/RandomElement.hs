module GenProg.RandomElement where

import System.Random
import Control.Applicative



randomElement _ _ [x] = x
randomElement p (r:rs) (x:xs) = if r < p then x else randomElement p rs xs

getRandomElement p xs = do
  rs <- randomRs (0,1::Double) <$> newStdGen
  return $ randomElement p rs xs
