{-|
Module      : GenProg.Types
Description : All of the types needed to represent a genetic program
Copyright   : (c) Jordan Medlock, 2015
                  University of New Mexico, 2015
License     : None
Maintainer  : medlock@unm.edu
Stability   : experimental
Portability : POSIX

-}
module GenProg.Types (
  Individual(..),
  Population(..),
  showPopulation,
  showIndividual
) where

import Types.Value
import GenProg.Test
import Data.List

-- | Represents an individual in the genetic program.
data Individual = Individual { name :: Value
                             -- ^ When the individual is represented in haskell
                             -- it is given a name usually /f/
                             , args :: [Value]
                             -- ^ Arguments to the function
                             , reqModules :: [String]
                             -- ^ Modules required for the individual
                             , func :: Value
                             -- ^ The code for the actual individual
                             } deriving (Show)

instance Eq Individual where
  a == b = func a == func b

-- | Represents a population of 'Individual's with the same 'Test' cases.
data Population = Population { individuals :: [Individual] -- ^ The individuals
                             , tests :: [Test] -- ^ The test cases
                             }

-- | Shows an 'Individual' in correct syntax within a let statement.
showIndividual ind = "let "++value (name ind)++" "++intercalate " " (map value $ args ind)++" = " ++ (showValue $ func ind) ++ " in "++value (name ind)

-- | Shows a 'Population' in correct syntax and creates a file that can be
-- executed and print the number of test cases passed.
showPopulation pop = map (showIndividualWith (tests pop)) $ individuals pop


showIndividualWith tests ind = unlines
  [ showModules $ nub $ reqModules ind
  , "tests = " ++ showTests tests
  , "function = " ++ showIndividual ind
  , "countPassing f = length $ filter ($ f) tests"
  , "main = print $ countPassing function"
  ]

showModules = concatMap (("import "++).(++"\n"))
