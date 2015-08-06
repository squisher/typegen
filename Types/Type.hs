{-# LANGUAGE TupleSections #-}
module Types.Type where

import Data.List

data Constraint = Constraint { unConstraint :: String } deriving (Read,Show,Eq, Ord)

data Type = Function Type Type
          | Tuple [Type]
          | Application Type Type
          | Concrete String
          | Polymorphic String [Constraint]
          deriving (Read,Show, Eq, Ord)


-- | Takes a type and displays it in correct Haskell syntax
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
