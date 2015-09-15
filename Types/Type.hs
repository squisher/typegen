{-# LANGUAGE TupleSections #-}
{-|
Module      : Types.Type
Description : Module containing types that represent the Haskell types
Copyright   : (c) Jordan Medlock, 2015
                  University of New Mexico, 2015
License     : None
Maintainer  : medlock@unm.edu
Stability   : experimental
Portability : POSIX

-}
module Types.Type (
  Constraint(..),
  Type(..),
  showType
) where

import Data.List

-- | Simple constraint newtype used to have type safety and documentatiopn
newtype Constraint = Constraint { unConstraint :: String
                                } deriving (Read,Show,Eq, Ord)

-- | A Haskell type
data Type = Function Type Type -- ^ First value is accepting type and second is return value
          | Tuple [Type] -- ^ Tuple type (eg. (Int,Int,Float), (a,b,c))
          | Application Type Type -- ^ Applied type (eg. m a, [Int])
          | Concrete String -- ^ Concrete type (eg. Int, Float, Bool)
          | Polymorphic String [Constraint] -- ^ Polymorphic type with or without constraints
          deriving (Read,Show, Eq, Ord)


-- | Takes a type and displays it with correct Haskell syntax
showType :: Type -> String
showType (Application (Concrete "[]") x) = "["++showType x++"]"
showType (Application f x) = showType f ++ " " ++ showType x
showType (Concrete x) = x
showType (Tuple xs) = "(" ++ intercalate ", " (map showType xs) ++ ")"
showType (Polymorphic a _) = a
showType f@(Function a b) = constStr ++ showInnerFunc a ++ " -> " ++ showOutterFunc b
  where consts = getConstraints f
        constStr = if null consts then "" else "(" ++ showConstraints consts ++ ") => "

-- | Takes a value in the middle of a function and displays it
showInnerFunc :: Type -> String
showInnerFunc (Function a b) = "(" ++ showInnerFunc a ++ " -> " ++ showOutterFunc b ++ ")"
showInnerFunc x = showType x

-- | Takes a value at the end of a function and displays it
showOutterFunc :: Type -> String
showOutterFunc (Function a b) = showInnerFunc a ++ " -> " ++ showOutterFunc b
showOutterFunc x = showType x

-- | Returns the constraints as (k,v) pairs for use when printing a function
getConstraints :: Type -> [(String,Constraint)]
getConstraints (Function a b) = nub $ getConstraints a ++ getConstraints b
getConstraints (Concrete x) = []
getConstraints (Tuple xs) = nub $ concatMap getConstraints xs
getConstraints (Application f x) = nub $ getConstraints f ++ getConstraints x
getConstraints (Polymorphic a cs) = nub $ map (a,) cs

-- | Shows a list of name constraint pairs for use in displaying functions
showConstraints :: [(String, Constraint)] -> String
showConstraints xs = intercalate ", " (map (\(a, Constraint s) -> s ++ " " ++ a) xs)
