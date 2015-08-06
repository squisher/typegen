module GenProg.Types where

import Types.Value
import GenProg.Test
import Data.List

data Individual = Individual { name :: Value, args :: [Value], reqModules :: [String], func :: Value } deriving (Show)

data Population = Population { individuals :: [Individual], tests :: [Test] }

showIndividual ind = "let "++value (name ind)++" "++intercalate " " (map value $ args ind)++" = " ++ (showValue $ func ind) ++ " in "++value (name ind)

showPopulation pop = map (showIndividualWith (tests pop)) $ individuals pop

showIndividualWith tests ind = unlines
  [ showModules $ nub $ reqModules ind
  , "tests = " ++ showTests tests
  , "function = " ++ showIndividual ind
  , "countPassing f = length $ filter ($ f) tests"
  , "main = print $ countPassing function"
  ]

showModules = concatMap (("import "++).(++"\n"))
