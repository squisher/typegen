{-|
Module      : Types.Value
Description : Types for representing a haskell value
Copyright   : (c) Jordan Medlock, 2015
                  University of New Mexico, 2015
License     : None
Maintainer  : medlock@unm.edu
Stability   : experimental
Portability : POSIX
-}
module Types.Value (
  Value(..),
  showValue,
  showTypeStructure
) where

import qualified Types.Type as T
import Control.Applicative

-- | Holds information representing Haskell values. Its made this way so that
-- you can easily combine values programmatically.  This is not necesarily complete.
-- It is true that you can use just these two to represent everything, however
-- you might want to create more.  A while ago I got rid of the Compose value
-- because I realised its not needed.  But I think it can be useful to add more
-- of these combinations so that it can be /faster/, /more robust/, /reach other
-- areas of Haskells type category/.  I think there might be functions the library
-- that you cant reach with just these two.  I cant prove it nor disprove it.
-- Just because you can create any function using these operations doesnt mean that
-- all of the library functions are reachable?????
data Value = Atom { typ :: T.Type
                  , value :: String
                  } -- ^ An atomic value (eg. 1, True, id, intercalate)
           | Apply { typ :: T.Type
                   , vF :: Value
                   , vX :: Value
                   } -- ^ An applied value (eg. id 1, length xs, ((elem 1) [1,2]))
           deriving (Read,Show)

instance Eq Value where
  (Atom _ a) == (Atom _ b) = a == b
  (Apply _ a b) == (Apply _ c d) = a == c && b == d
  a == b = showValue a == showValue b

-- | Shows a value in correct haskell syntax. Its not very human readable though,
-- you might want to improve it...
showValue :: Value -> String
showValue (Atom typ value) = value
showValue (Apply typ f x) = "("++showValue f ++ " " ++ showValue x++")"

-- | Shows a value in __/probably/__ correct haskell syntax.
-- I wouldnt trust it though. Its mostly for debugging perposes.
showTypeStructure :: Value -> String
showTypeStructure (Atom typ value) = value ++ " :: " ++ T.showType typ
showTypeStructure (Apply typ f x) = "(("++showTypeStructure f++") ("++showTypeStructure x++") :: "++T.showType typ++")"
