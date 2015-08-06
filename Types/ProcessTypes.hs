{-# LANGUAGE TupleSections #-}
module Types.ProcessTypes where

import           Control.Applicative
import           Control.Monad
import           System.IO.Unsafe
import           Data.List
import           Data.String
import Search.Instances
import Types.Type
import GenProg.Types
import Types.Value
import Data.Bool (bool)
import Debug.Trace

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

-- coerce :: Type -> Type -> Maybe Type

equivalentTo (Polymorphic a xs) (Polymorphic b ys) = xs == ys
equivalentTo (Concrete a) (Concrete b) = a == b
equivalentTo (Function a b) (Function c d) = a `equivalentTo` c && b `equivalentTo` d
equivalentTo (Application a b) (Application c d) = a `equivalentTo` c && b `equivalentTo` d
equivalentTo (Tuple xs) (Tuple ys) = and $ zipWith equivalentTo xs ys
equivalentTo x y = False

strings = map unConstraint
-- | Apply the second argument to the first, see if they fit, if they do
-- Return the resulting value `apply f x`
apply :: Type -> Type -> Maybe Type
apply f@(Function a b) x = do
  changes <- changesNeeded x a
  let newX = replace changes x
  let newB = replace changes b
  guard $ newX `equivalentTo` a
  guard $ newB `equivalentTo` b
  return newB

--     actual `usableAs` expected
usableAs :: Type -> Type -> Bool
usableAs a b | a == b                         = True
usableAs (Polymorphic _ []) _                 = True
usableAs a (Polymorphic _ cs)                 = a `matchesConstraints` cs
usableAs (Polymorphic _ cs) x                 = x `hasAConstraintIn` cs
usableAs (Tuple as) (Tuple bs)                = and (zipWith usableAs as bs)
usableAs (Application a b) (Application c d)  = and (zipWith usableAs [c,d] [a,b])
usableAs (Function a b) (Function c d)        = and (zipWith usableAs [c,d] [a,b])
usableAs _ _                                  = False

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
findConstrainsts :: String -> [String]
findConstrainsts str = unsafePerformIO $ do
  instances <- getInstances str
  return $ map clas instances

-- | Returns whether or not a type matches a list of constraints
matchesConstraints :: Type -> [Constraint] -> Bool
matchesConstraints (Concrete a) cs = (length $ strings cs `intersect` (findConstrainsts a)) == (length cs)
matchesConstraints (Polymorphic _ xs) cs = (length $ cs `intersect` xs) == (length cs)
matchesConstraints _ [] = True
matchesConstraints _ _ = False

hasAConstraintIn :: Type -> [Constraint] -> Bool
hasAConstraintIn (Concrete a) cs = not $ null (strings cs `intersect` (findConstrainsts a))
hasAConstraintIn (Polymorphic _ xs) cs = not $ null $ cs `intersect` xs
hasAConstraintIn _ [] = True
hasAConstraintIn _ _ = False

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

replaceWith :: Type -> Type -> Type -> Maybe Type
f@(Function a b) `replaceWith` g@(Function c d) | g `usableAs` f = (a `replaceWith` c) <=< (b `replaceWith` d)
f@(Application a b) `replaceWith` g@(Application c d) | g `usableAs` f = (a `replaceWith` c) <=< (b `replaceWith` d)
f@(Tuple as) `replaceWith` g@(Tuple bs) | g `usableAs` f = (as `replaceAllWith` bs)
p@(Polymorphic n cs) `replaceWith` c | c `usableAs` p = (Just . mapType (replaceInd p c))
a@(Concrete _) `replaceWith` b@(Concrete _) | a == b = Just
a@(Concrete _) `replaceWith` b@(Polymorphic _ _) | b `usableAs` a = Just
a@(Application _ _) `replaceWith` b@(Polymorphic _ _) = const Nothing -- im not sure if this is the right move???
a `replaceWith` b@(Polymorphic _ _) = const Nothing -- im not sure if this is the right move???
replaceWith a b = error $ "ProcessTypes.hs: failed with: " ++ show a ++ " `replaceWith` " ++ show b


replaceAllWith :: [Type] -> [Type] -> Type -> Maybe Type
replaceAllWith (x:xs) (y:ys) = replaceWith x y >=> replaceAllWith xs ys
replaceAllWith [] [] = Just

applyValues a@(Atom t1 _) b@(Atom t2 _) = ($ b) <$> ($ a) <$> (Apply <$> apply t1 t2)
applyValues a@(Atom t1 _) b@(Apply t2 _ _) = ($ b) <$> ($ a) <$> (Apply <$> apply t1 t2)
applyValues a@(Apply t1 _ _) b@(Apply t2 _ _) = ($ b) <$> ($ a) <$> (Apply <$> apply t1 t2)
applyValues a@(Apply t1 _ _) b@(Atom t2 _) = ($ b) <$> ($ a) <$> (Apply <$> apply t1 t2)

replace :: [(String,Type)] -> Type -> Type
replace table (Function a b) = (Function (replace table a) (replace table b))
replace table (Application a b) = (Application (replace table a) (replace table b))
replace table (Tuple xs) = (Tuple $ map (replace table) xs)
replace table a@(Concrete x) = a
replace table a@(Polymorphic s xs) = maybe a id $ lookup s table

(Just a) <> (Just b) = Just $ a ++ b
Nothing <> a = Nothing
a <> Nothing = Nothing

isEveryIn (x:xs) ys = x `elem` ys && isEveryIn xs ys
isEveryIn [] _ = True

catJusts :: [Maybe a] -> Maybe [a]
catJusts (Just x:xs) = (x :) <$> catJusts xs
catJusts [] = Just []
catJusts _ = Nothing

reconcile :: Type -> Type -> Maybe Type
reconcile a b | a == b = Just a
reconcile (Polymorphic a xs) (Polymorphic b ys) | a == b = Just (Polymorphic a (if length xs > length ys then xs else ys))
reconcile (Polymorphic _ cs) a | a `matchesConstraints` cs = Just a
reconcile a b = Nothing

confirm :: [(String,Type)] -> Maybe [(String,Type)]
confirm ((s,t):xs) = case lookup s xs of
                        Nothing -> ((s,t) :) <$> confirm xs
                        (Just t2) -> do
                          f <- reconcile t t2
                          b <- confirm xs
                          return $ (s,f) : b
confirm [] = Just []

-- answer `usableAs` question =
changesNeeded' :: Type -> Type -> Maybe [(String,Type)]
changesNeeded' (Function a b) (Function c d) = confirm =<< changesNeeded' a c <> changesNeeded' b d
changesNeeded' (Application a b) (Application c d) = confirm =<< changesNeeded' a c <> changesNeeded' b d
changesNeeded' (Tuple xs) (Tuple ys) = confirm . concat =<< catJusts (zipWith changesNeeded' xs ys)
changesNeeded' (Concrete _) (Concrete _) = Just []
changesNeeded' (Polymorphic s xs) q@(Polymorphic _ ys) | all (`elem`ys) xs = Just [(s,q)]
changesNeeded' (Polymorphic s xs) q = Just [(s,q)]
changesNeeded' a q = Nothing

changesNeeded a b = nub <$> changesNeeded' a b

-- Changes needed should work well enough to be used with replace
