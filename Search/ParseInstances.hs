{-|
Module      : Search.ParseInstances
Description : Parses instance declarations
Copyright   : (c) Jordan Medlock, 2015
                  University of New Mexico, 2015
License     : None
Maintainer  : medlock@unm.edu
Stability   : experimental
Portability : POSIX
-}
module Search.ParseInstances (
  parseInstances,
  Instance(..)
) where

import Control.Applicative ((<$>),(<*>))
import Language.Haskell.Syntax
import Language.Haskell.Parser
import Language.Haskell.Pretty
import Data.Either

import Types.Type

-- | Simple Instance data type
data Instance = Instance { clas :: String -- ^ Typeclass that this is an instance of
                         , types :: [Type] -- ^ Types that have an instance for that class
                         } deriving (Show, Eq)

-- | Parses a string and returns the instance declarations
parseInstances :: String -> Either String [Instance]
parseInstances str = case parseModule (str++"\n") of
  (ParseOk mod) -> interpretModule mod
  (ParseFailed loc str) -> Left $ show loc ++ " " ++ str
-- "\n" because comments arent ended otherwise


interpretModule :: HsModule -> Either String [Instance]
interpretModule (HsModule _ _ _ _ [] ) = Right []
interpretModule (HsModule _ _ _ _ decls ) = Right $ rights $ map interpretDeclaration decls

interpretDeclaration :: HsDecl -> Either String Instance
interpretDeclaration (HsInstDecl _ hscontext hsqname hstypes _) =
  Instance (nameFromQname hsqname)
    <$> interpretTypes hscontext hstypes
interpretDeclaration _ = Left "Wrong decl"

nameToString :: HsName -> String
nameToString (HsIdent str) = str
nameToString (HsSymbol str) = "("++str++")"

nameFromQname :: HsQName -> String
nameFromQname (Qual (Module mod) hsname) = mod ++ "." ++ nameToString hsname
nameFromQname (UnQual hsname) = nameToString hsname
nameFromQname (Special HsUnitCon) = "()"
nameFromQname (Special HsListCon) = "[]"
nameFromQname (Special HsFunCon) = "->"
nameFromQname (Special (HsTupleCon n)) = "("++replicate n ','++")"
nameFromQname (Special HsCons) = "(:)"

interpretTypes :: HsContext -> [HsType] -> Either String [Type]
interpretTypes table types = mapM (toTyp table) types

toTyp :: HsContext -> HsType -> Either String Type
toTyp table (HsTyFun t1 t2) = Function <$> toTyp table t1 <*> toTyp table t2
toTyp table (HsTyTuple ts) = Tuple <$> mapM (toTyp table) ts
toTyp table (HsTyApp t1 t2) = Application <$> toTyp table t1 <*> toTyp table t2
toTyp table a@(HsTyVar hsname) = Polymorphic (nameToString hsname)
                                <$> lookupName table a
toTyp table (HsTyCon hsqname) = Right $ Concrete $ nameFromQname hsqname


-- | Looks up the given constraint in the table given
lookupName :: [(HsQName, [HsType])] -> HsType -> Either String [Constraint]
lookupName ((qName,types):xs) name
  | name `elem` types = (Constraint (nameFromQname qName) :) <$> lookupName xs name
  | otherwise = lookupName xs name
lookupName [] _ = Right []
