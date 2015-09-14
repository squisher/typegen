module Types.Value where

import qualified Types.Type as T
import Control.Applicative

data Value = Atom { typ :: T.Type, value :: String }
           | Apply { typ :: T.Type, vF :: Value, vX :: Value }
           deriving (Read,Show)

instance Eq Value where
  (Atom _ a) == (Atom _ b) = a == b
  (Apply _ a b) == (Apply _ c d) = a == c && b == d
  a == b = showValue a == showValue b

showValue :: Value -> String
showValue (Atom typ value) = value
showValue (Apply typ f x) = "("++showValue f ++ " " ++ showValue x++")"

showTypeStructure :: Value -> String
showTypeStructure (Atom typ value) = value ++ " :: " ++ T.showType typ
showTypeStructure (Apply typ f x) = "(("++showTypeStructure f++") ("++showTypeStructure x++") :: "++T.showType typ++")"
