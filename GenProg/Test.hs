{-|
Module      : GenProg.Test
Description : The Test type and constructors
Copyright   : (c) Jordan Medlock, 2015
                  University of New Mexico, 2015
License     : None
Maintainer  : medlock@unm.edu
Stability   : experimental
Portability : POSIX
-}
module GenProg.Test (
  Test(..),
  showTests,
  mkUnaryTests
) where

import Data.List
import System.Random

-- | Represents a single test case as a string of Haskell code that holds a
-- function that takes the function in question and returns a boolean whether
-- or not it succeeds.
data Test = Test String deriving (Show)

-- | Creates a list of random unary tests where the input is in the range given
-- and checks equality by applying f to that value
mkUnaryTests :: (Random a, Show a, Show b) => (a -> b) -> (a, a) -> Int -> IO [Test]
mkUnaryTests f range n = do
  gen <- newStdGen
  return $ map (mkUnaryTest f) $ take n $ randomRs range gen

mkUnaryTest :: (Show a, Show b) => (a -> b) -> a -> Test
mkUnaryTest f i = Test $ "\\f -> f " ++ show i ++ " == " ++ show (f i)

-- | Shows a list of tests as correct Haskell code
showTests :: [Test] -> String
showTests tests = "[" ++ (intercalate "," $ map (\(Test s) -> s) tests) ++ "]"
