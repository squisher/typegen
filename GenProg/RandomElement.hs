{-|
Module      : GenProg.RandomElement
Description : A utility module that provides functions that randomly select elements from lists
Copyright   : (c) Jordan Medlock, 2015
                  University of New Mexico, 2015
License     : None
Maintainer  : medlock@unm.edu
Stability   : experimental
Portability : POSIX

This module was made with the hopes of creating functions which randomly select
one or more elements from a list with the assumption that the list is infinite.
Whether or not is succeeds I'm not sure.

You cant select an element from an infinite list with uniform probability so it
uses a __(I forgot the name)__ probability distribution.  It basically traverses
the list and flips an uneven coin and if its true it returns that element
otherwise it continues to traverse.
-}
module GenProg.RandomElement (
  getRandomElement,
  getRandomElements
) where

import System.Random
import Control.Applicative
import Data.Maybe



randomElement _ _ [x] = x
randomElement p (r:rs) (x:xs) = if r < p then x else randomElement p rs xs

randomElements :: Double -> [Double] -> [a] -> [a]
randomElements p rs = catMaybes . zipWith (\r x -> if r < p then Just x else Nothing) rs

-- | Returns an element from 'xs' with special coin probability 'p'
-- From what I know p = 1/µ which means that if you assume your list is 'x' wide
-- then you should use p = 1 / (0.5x). If it reaches the end of the list it
-- it returns the last element.
getRandomElement p xs = do
  rs <- randomRs (0,1::Double) <$> newStdGen
  return $ randomElement p rs xs

-- | Returns an infinite list of elements from 'xs' with special coin probability 'p'
-- From what I know p = 1/µ which means that if you assume your list is 'x' wide
-- then you should use p = 1 / (0.5x). If it reaches the end of the list it
-- it returns the last element.
getRandomElements p xs = do
  rs <- randomRs (0,1::Double) <$> newStdGen
  return $ randomElements p rs xs
