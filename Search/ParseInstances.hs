module Search.ParseInstances where

import Control.Applicative ((<$>),(<*>))
import Language.Haskell.Syntax
import Language.Haskell.Parser
import Language.Haskell.Pretty

import Types.Type
import Data.Either


data Instance = Instance { clas :: String, types :: [Type] } deriving (Show, Eq)

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
