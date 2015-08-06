{-# LANGUAGE TupleSections #-}
module GenProg.Mutations where
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

checkEnvVars :: [Value] -> Type -> [Value]
checkEnvVars xs t = filter ((`usableAs`t) . typ) xs

getEnvironment :: Individual -> [Value]
getEnvironment ind = name ind : args ind ++ map (Atom (Polymorphic "abcd" [Constraint "Num"]) . show) [0..9]
                                         ++ [(Atom (Application (Concrete "[]") (Polymorphic "abcd" [])) "[]")]

interleave :: [a] -> [a] -> [a] -> [a]
interleave [] [] [] = []
interleave xs ys zs = (take 1 xs) ++ (take 1 ys) ++ (take 1 zs)
                      ++ interleave (drop 1 xs) (drop 1 ys) (drop 1 zs)
-- Helpers ^^^^^^^^^^^^^^^


mutations :: Individual -> IO [Individual]
mutations ind = do
  let env = getEnvironment ind
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
  let envs = map ([],) $ checkEnvVars env appliedType
  let fs = map testFunctionToClosure matches ++ envs
  let mas = map (\(f,s) -> (f,) <$> applyValues s v) fs
  return $ catMaybes mas
applicationsToValue env (Apply t v1 v2) = do
  newV1s <- filter (((typ v1)`usableAs`).typ.snd) <$> mutationsToValue env v1
  newV2s <- filter (((typ v2)`usableAs`).typ.snd) <$> mutationsToValue env v2
  return [(m1 `union` m2, Apply t v1 v2) | (m1, v1) <- newV1s, (m2, v2) <- newV2s  ]

-- Applications ^^^^^^^^^^^^^^^^

-- Modifications vvvvvvvvvvv

modificationsToValue :: [Value] -> Value -> IO [([String],Value)]
modificationsToValue env v@(Atom _ _) = do
  matches <- hoogle (typ v)
  let envs = map ([],) $ checkEnvVars env (typ v)
  let fs = map testFunctionToClosure matches ++ envs
  return fs
modificationsToValue env v@(Apply t v1 v2) = do
  newV1s <- filter (((typ v1)`usableAs`).typ.snd) <$> mutationsToValue env v1
  newV2s <- filter (((typ v2)`usableAs`).typ.snd) <$> mutationsToValue env v2
  matches <- hoogle (typ v)
  let envs = map ([],) $ checkEnvVars env (typ v)
  let fs = map testFunctionToClosure matches ++ envs
  return $ fs ++ [(m1 `union` m2, Apply t v1 v2) | (m1, v1) <- newV1s, (m2, v2) <- newV2s]

-- Modifications ^^^^^^^^^^^^^^


-- UnApplications vvvvvvvvvvv

unApplicationsToValue :: [Value] -> Value -> IO [([String],Value)]
unApplicationsToValue env v@(Atom _ _) = return []
unApplicationsToValue env v@(Apply t v1 v2) = do
  if (typ v2) `usableAs` t
    then filter (((typ v1)`usableAs`).typ.snd) <$> mutationsToValue env v2
    else return []
-- UnApplications ^^^^^^^^^^^^^^



-- unApplicationsToValue
