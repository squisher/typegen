{-# LANGUAGE TupleSections #-}
{-|
Module      : GenProg.Mutations
Description : Finds lists of mutations to a 'Value'
Copyright   : (c) Jordan Medlock, 2015
                  University of New Mexico, 2015
License     : None
Maintainer  : medlock@unm.edu
Stability   : experimental
Portability : POSIX
-}
module GenProg.Mutations (
  mutations
) where
import           Control.Applicative
import           Data.List
import           Data.Maybe
import           GenProg.Types
import           Search.Hoogle
import           System.Random
import           Types.ProcessTypes
import           Types.TestFunction
import           Types.Type
import           Types.Value
import Control.Monad.Trans.Maybe
import Control.Monad
-- Helpers vvvvvvvvv

nextLabel :: IO String
nextLabel = do
  a <- getStdRandom (randomR ('a','z'))
  b <- getStdRandom (randomR ('a','z'))
  c <- getStdRandom (randomR ('a','z'))
  return $ a:b:c:[]

testFunctionToClosure :: TestFunction -> ([String],Value)
testFunctionToClosure tf = (maybeToList $ modName tf, funcValue tf)

combine :: [([String],Value)] -> Individual -> [Individual]
combine xs ind = ind : map (\(mods, val) -> Individual (name ind) (args ind) (mods ++ reqModules ind) val) xs

checkEnvVars :: [Value] -> Type -> IO [Value]
checkEnvVars xs t = filterM ((`usableAs`t) . typ) xs

nil = (Atom (Application (Concrete "[]") (Polymorphic "abcd" [])) "[]")
numbers = map (Atom (Polymorphic "abcd" [Constraint "Num"]) . show) [0..9]

getEnvironment :: Individual -> [Value]
getEnvironment ind = nil : name ind : args ind -- ++ numbers

interleave :: [a] -> [a] -> [a] -> [a]
interleave [] [] [] = []
interleave xs ys zs = (take 1 xs) ++ (take 1 ys) ++ (take 1 zs)
                      ++ interleave (drop 1 xs) (drop 1 ys) (drop 1 zs)
-- Helpers ^^^^^^^^^^^^^^^

-- | Generates a __massive__ list of mutations to an individual. These are all
-- the concevable mutations to a single 'Individual'
mutations :: Individual -> IO [Individual]
mutations ind = do
  let env = getEnvironment ind
  -- mapM_ print env
  closure <- mutationsToValue env (func ind)
  return $ combine closure ind

mutationsToValue env val = do
  apps <- applicationsToValue env val
  mods <- modificationsToValue env val
  unas <- unApplicationsToValue env val
  return $ interleave apps mods unas


-- Applications vvvvvvvvvvv

applicationsToValue :: [Value] -> Value -> IO [([String],Value)]
applicationsToValue env v@(Atom _ _) = do
  let vTyp = typ v
  lab <- nextLabel
  let appliedType = (Function vTyp vTyp)
  matches <- hoogle appliedType
  envs <- map ([],) <$> checkEnvVars env appliedType
  let fs = map testFunctionToClosure matches ++ envs
  mas <- mapM (\(f,s) -> runMaybeT $ (f,) <$> applyValues s v) fs
  return $ catMaybes mas
applicationsToValue env (Apply t v1 v2) = do
  newV1s <- filterM (((typ v1)`usableAs`).typ.snd) =<< mutationsToValue env v1
  newV2s <- filterM (((typ v2)`usableAs`).typ.snd) =<< mutationsToValue env v2
  return [(m1 `union` m2, Apply t v1 v2) | (m1, v1) <- newV1s, (m2, v2) <- newV2s  ]

-- Applications ^^^^^^^^^^^^^^^^

-- Modifications vvvvvvvvvvv

modificationsToValue :: [Value] -> Value -> IO [([String],Value)]
modificationsToValue env v@(Atom _ _) = do
  matches <- hoogle (typ v)
  envs <- map ([],) <$> checkEnvVars env (typ v)
  let fs = map testFunctionToClosure matches ++ envs
  return fs
modificationsToValue env v@(Apply t v1 v2) = do
  newV1s <- filterM (((typ v1)`usableAs`).typ.snd) =<< mutationsToValue env v1
  newV2s <- filterM (((typ v2)`usableAs`).typ.snd) =<< mutationsToValue env v2
  matches <- hoogle (typ v)
  envs <- map ([],) <$> checkEnvVars env (typ v)
  let fs = map testFunctionToClosure matches ++ envs
  return $ fs ++ [(m1 `union` m2, Apply t v1 v2) | (m1, v1) <- newV1s, (m2, v2) <- newV2s]

-- Modifications ^^^^^^^^^^^^^^


-- UnApplications vvvvvvvvvvv

unApplicationsToValue :: [Value] -> Value -> IO [([String],Value)]
unApplicationsToValue env v@(Atom _ _) = return []
unApplicationsToValue env v@(Apply t v1 v2) = do
  b <- (typ v2) `usableAs` t
  if b
    then filterM (((typ v1)`usableAs`).typ.snd) =<< mutationsToValue env v2
    else return []
-- UnApplications ^^^^^^^^^^^^^^



-- unApplicationsToValue
