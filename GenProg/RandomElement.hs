module GenProg.RandomElement where

import System.Random
import Control.Applicative
import Data.Maybe



randomElement _ _ [x] = x
randomElement p (r:rs) (x:xs) = if r < p then x else randomElement p rs xs

randomElements :: Double -> [Double] -> [a] -> [a]
randomElements p rs = catMaybes . zipWith (\r x -> if r < p then Just x else Nothing) rs


getRandomElement p xs = do
  rs <- randomRs (0,1::Double) <$> newStdGen
  return $ randomElement p rs xs

getRandomElements p xs = do
  rs <- randomRs (0,1::Double) <$> newStdGen
  return $ randomElements p rs xs
