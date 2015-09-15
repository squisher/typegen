{-# LANGUAGE TupleSections #-}
{-|
Module      : Types.ProcessTypes
Description : A ton of functions used to process types into other types
Copyright   : (c) Jordan Medlock, 2015
                  University of New Mexico, 2015
License     : None
Maintainer  : medlock@unm.edu
Stability   : experimental
Portability : POSIX
-}
module Types.ProcessTypes (
  usableAs,
  applyValues,
  apply
) where

import           Control.Applicative
import           Control.Monad
import           System.IO.Unsafe
import           Data.List
import           Data.String

import Search.Instances
import Types.Type
-- import GenProg.Types
import Types.Value

import Data.Bool (bool)
import Debug.Trace
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Data.Monoid
import Data.Maybe
--------------------------
-- Type modification stuff
--------------------------


{-

Some thoughts on the type conversion

(+) :: (Num a) => a -> a -> a

applied to an Int becomes:

(+) :: Int -> Int -> Int

I think that is the point of ?coerce? it takes the first Polymorphic type and

The values that you are applying your function to always have a stronger type
signature than the fucntion

-}



equivalentTo (Polymorphic a xs) (Polymorphic b ys) = xs == ys
equivalentTo (Concrete a) (Concrete b) = a == b
equivalentTo (Function a b) (Function c d) = a `equivalentTo` c && b `equivalentTo` d
equivalentTo (Application a b) (Application c d) = a `equivalentTo` c && b `equivalentTo` d
equivalentTo (Tuple xs) (Tuple ys) = and $ zipWith equivalentTo xs ys
equivalentTo x y = False

strings = map unConstraint
-- | Apply the second argument to the first, see if they fit, if they do
-- Return the resulting value `apply f x`
apply :: Type -> Type -> MaybeT IO Type
apply f@(Function a b) x = do
  changes <- changesNeeded x a
  let newX = replace changes x
  let newB = replace changes b
  lift $ if a == (Concrete "Bool")
        then do
          putStrLn $ "Heres what I am getting: " ++ showType f ++ " <applied to> " ++ show x
          putStrLn $ "And heres newX: " ++ showType newX ++ " and newB: " ++ showType newB
          putStrLn $ "And these are the changes: "
          mapM_ (print) changes
        else return ()
  guard $ newX `equivalentTo` a
  guard $ newB `equivalentTo` b
  return newB

-- | This function defines when a type definition is usable in the place of another one.
-- Keep in mind that a `usableAs` b does not necesarily equal b `usableAs` a.
usableAs :: Type -> Type -> IO Bool
usableAs a b | a == b                         = return True
usableAs (Polymorphic _ []) _                 = return True
usableAs a (Polymorphic _ cs)                 = a `matchesConstraints` cs
usableAs (Polymorphic _ cs) x                 = x `hasAConstraintIn` cs
usableAs (Tuple as) (Tuple bs)                = and <$> (zipWithM usableAs as bs)
usableAs (Application a b) (Application c d)  = and <$> (zipWithM usableAs [c,d] [a,b])
usableAs (Function a b) (Function c d)        = and <$> (zipWithM usableAs [c,d] [a,b])
usableAs _ _                                  = return False

-- | List of known Concrete type - instances
-- TODO get this from the DataBase in Hoogle
constraintPairs :: [([String],[String])]
constraintPairs = [(["Int","Integer"], ["Num","Enum","Eq","Integral","Data"
                                       ,"Ord","Read","Real","Show","Ix"
                                       ,"Typeable","Bits","PrintfArg"])
                  ,(["Float","Double"], ["Num","Enum","Eq","Data","Ord","Read"
                                        ,"Real","Show","Typeable","Bits"
                                        ,"PrintfArg","Floating","Fractional"
                                        ,"RealFloat","RealFrac"])]

-- | Returns the list of constraints for any concrete type
findConstraints :: String -> IO [String]
findConstraints str = do
  instances <- getInstances str
  return $ map clas instances

-- | Returns whether or not a type matches a list of constraints
matchesConstraints :: Type -> [Constraint] -> IO Bool
matchesConstraints (Concrete a) cs = do
  constraints <- findConstraints a
  return $ (length $ strings cs `intersect` constraints) == (length cs)
matchesConstraints (Polymorphic _ xs) cs = return $ (length $ cs `intersect` xs) == (length cs)
matchesConstraints _ [] = return $ True
matchesConstraints _ _ = return $ False

hasAConstraintIn :: Type -> [Constraint] -> IO Bool
hasAConstraintIn (Concrete a) cs = do
  constraints <- findConstraints a
  return $ not $ null (strings cs `intersect` constraints)
hasAConstraintIn (Polymorphic _ xs) cs = return $ not $ null $ cs `intersect` xs
hasAConstraintIn _ [] = return $ True
hasAConstraintIn _ _ = return $ False

mapType :: (Type -> Type) -> Type -> Type
mapType f (Function a b) = (Function (mapType f a) (mapType f b))
mapType f (Application a b) = (Application (mapType f a) (mapType f b))
mapType f (Tuple xs) = (Tuple $ map (mapType f) xs)
mapType f x = f x

replaceInd :: Type -> Type -> Type -> Type
replaceInd (Polymorphic a _) x (Polymorphic b _) | b == a = x
replaceInd (Concrete a) x (Concrete b) | b == a =           x
replaceInd a x b | a == b =                                 x
replaceInd a x b =                                          b


replaceWithHelper :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> MaybeT IO Type
replaceWithHelper g f a b c d inn = do
  guard =<< (lift $ g `usableAs` f)
  (a `replaceWith` c) <=< (b `replaceWith` d) $ inn


replaceWith :: Type -> Type -> Type -> MaybeT IO Type
replaceWith f@(Function a b) g@(Function c d) inn = replaceWithHelper g f a b c d inn
replaceWith f@(Application a b) g@(Application c d) inn = replaceWithHelper g f a b c d inn
replaceWith f@(Tuple as) g@(Tuple bs) inn = do
  guard =<< lift (g `usableAs` f)
  (as `replaceAllWith` bs) inn
replaceWith p@(Polymorphic n cs) c inn = do
  guard =<< lift (c `usableAs` p)
  return $ mapType (replaceInd p c) inn
replaceWith a@(Concrete _) b@(Concrete _) inn | a == b = return inn
replaceWith a@(Concrete _) b@(Polymorphic _ _) inn = do
  guard =<< lift (b `usableAs` a)
  return inn
replaceWith a@(Application _ _) b@(Polymorphic _ _) _ = mzero -- im not sure if this is the right move???
replaceWith a b@(Polymorphic _ _) _ = mzero -- im not sure if this is the right move???
replaceWith a b _ = error $ "ProcessTypes.hs: failed with: " ++ show a ++ " `replaceWith` " ++ show b


replaceAllWith :: [Type] -> [Type] -> Type -> MaybeT IO Type
replaceAllWith (x:xs) (y:ys) = replaceWith x y >=> replaceAllWith xs ys
replaceAllWith [] [] = return

-- | Takes two values and uses 'apply' to apply their two types together then
-- combine them with 'Apply'.
applyValues a@(Atom t1 _) b@(Atom t2 _)       = (\t -> Apply t a b) <$> apply t1 t2
applyValues a@(Atom t1 _) b@(Apply t2 _ _)    = (\t -> Apply t a b) <$> apply t1 t2
applyValues a@(Apply t1 _ _) b@(Apply t2 _ _) = (\t -> Apply t a b) <$> apply t1 t2
applyValues a@(Apply t1 _ _) b@(Atom t2 _)    = (\t -> Apply t a b) <$> apply t1 t2

replace :: [(String,Type)] -> Type -> Type
replace table (Function a b) = (Function (replace table a) (replace table b))
replace table (Application a b) = (Application (replace table a) (replace table b))
replace table (Tuple xs) = (Tuple $ map (replace table) xs)
replace table a@(Concrete x) = a
replace table a@(Polymorphic s xs) = maybe a id $ lookup s table


isEveryIn (x:xs) ys = x `elem` ys && isEveryIn xs ys
isEveryIn [] _ = True

reconcile :: Type -> Type -> MaybeT IO Type
reconcile a b | a == b = return a
reconcile (Polymorphic a xs) (Polymorphic b ys) | a == b = return (Polymorphic a (if length xs > length ys then xs else ys))
reconcile (Polymorphic _ cs) a = do
  guard =<< lift (a `matchesConstraints` cs)
  return a
reconcile a b = mzero

confirm :: [(String,Type)] -> MaybeT IO [(String,Type)]
confirm ((s,t):xs) = case lookup s xs of
                        Nothing -> ((s,t) :) <$> confirm xs
                        (Just t2) -> do
                          f <- reconcile t t2
                          b <- confirm xs
                          return $ (s,f) : b
confirm [] = return []

-- answer `usableAs` question =
changesNeeded' :: Type -> Type -> MaybeT IO [(String,Type)]
changesNeeded' (Function a b) (Function c d) = do
  acChanges <- changesNeeded' a c
  bdChanges <- changesNeeded' b d
  confirm $ acChanges <> bdChanges
changesNeeded' (Application a b) (Application c d) = do
  acChanges <- changesNeeded' a c
  bdChanges <- changesNeeded' b d
  confirm $ acChanges <> bdChanges
changesNeeded' (Tuple xs) (Tuple ys) = do
  changes <- concat <$> zipWithM changesNeeded' xs ys
  confirm changes
changesNeeded' (Concrete x) (Concrete y)
  | x == y = return []
changesNeeded' (Polymorphic s xs) q@(Polymorphic _ ys)
  | all (`elem`ys) xs = return [(s,q)]
changesNeeded' (Polymorphic s xs) q@(Concrete y) = do
  instances <- map (Constraint . clas) <$> lift (getInstances y)
  guard $ all (`elem` instances) xs
  return [(s,q)]
changesNeeded' (Polymorphic s xs) q = return [(s,q)]
changesNeeded' a q = mzero

changesNeeded a b = nub <$> changesNeeded' a b

-- Changes needed should work well enough to be used with replace
