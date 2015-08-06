module Types.TestFunction (TestFunction(..),modName,funcValue,parseTestFunction) where

import Types.Value as V
import Types.Type as T
import Control.Applicative ((<$>),(<*>))
import Language.Haskell.Syntax
import Language.Haskell.Parser
import Data.List
import Data.Either
import Data.String
import Debug.Trace

data TestFunction = TestFunction (Maybe String) Value deriving (Read,Show, Eq)

modName (TestFunction x _) = x
funcValue (TestFunction _ x) = x

instance IsString TestFunction where
  fromString = either (const undefined) id . parseTestFunction

instance IsString Value where
  fromString = funcValue . fromString

instance IsString Type where
  fromString = typ . fromString

-- | Parses a string and turns it into a TestFunction with a Left error
parseTestFunction :: String -> Either String TestFunction
parseTestFunction s = case parseModule s of
  (ParseOk x) -> toTestFunction x
  x -> Left (show x ++ " the string is "++s)

-- | Takes a Module from parseModule and turns it into a TestFunction
toTestFunction :: HsModule -> Either String TestFunction
toTestFunction (HsModule _ _ _ _ (dec:_)) = fromDecl dec
toTestFunction (HsModule _ _ _ _ []) = Left "Empty declarations"

-- | Takes a Declaration and returns a TestFunction
fromDecl :: HsDecl -> Either String TestFunction
fromDecl (HsTypeSig _ (name:_) typ) = (Right . TestFunction Nothing) =<< genValue name typ
fromDecl (HsTypeSig _ _ typ) = Left "Not enough names"
fromDecl x = Left ("Wrong type of declaration: "++show x)

-- | Takes a Name and a qualified type and returns a Value
genValue :: HsName -> HsQualType -> Either String Value
genValue (HsIdent name) (HsQualType context typ) = (Right . flip Atom name) =<< genType context typ
genValue (HsSymbol name) (HsQualType context typ) = (Right . flip Atom ("("++name++")")) =<< genType context typ

-- | Takes a table of qualified names and types and turns it into a Type
genType :: [(HsQName, [HsType])] -> HsType -> Either String Type
genType table (HsTyFun a b) = Function <$> genType table a <*> genType table b
genType table (HsTyTuple as) = Tuple <$> mapM (genType table) as
genType table (HsTyApp a b) = Application <$> genType table a <*> genType table b
genType table a@(HsTyVar n) = Polymorphic (nameToString n) <$> lookupContraints table a
genType table (HsTyCon a) = Right $ Concrete $ qnameToString a

-- | Looks up the given constraint in the table given
lookupContraints :: [(HsQName, [HsType])] -> HsType -> Either String [Constraint]
lookupContraints ((qName,types):xs) name
  | name `elem` types = (Constraint (qnameToString qName) :) <$> lookupContraints xs name
  | otherwise = lookupContraints xs name
lookupContraints [] _ = Right []

-- | Takes an HsName and turns it into a string
nameToString :: HsName -> String
nameToString (HsIdent name) = name
nameToString (HsSymbol name) = "("++name++")"

-- | Takes a qualified name and turns it into a string
qnameToString :: HsQName -> String
qnameToString (Qual (Module m) name) = m++"."++nameToString name
qnameToString (UnQual name) = nameToString name
qnameToString (Special HsUnitCon) = "()"
qnameToString (Special HsListCon) = "[]"
qnameToString (Special HsFunCon) = "->"
qnameToString (Special (HsTupleCon n)) = "("++replicate n ','++")"
qnameToString (Special HsCons) = "(:)"

-- | Takes a type and turns it into a String
typeToString :: HsType -> String
typeToString (HsTyFun a b) = typeToString a ++ " -> " ++ typeToString b
typeToString (HsTyTuple as) = "("++intercalate "," (map typeToString as) ++ ")"
typeToString (HsTyApp a b) = typeToString a ++ " " ++ typeToString b
typeToString (HsTyVar a) = nameToString a
typeToString (HsTyCon a) = qnameToString a

main = print $ parseTestFunction "undefined :: (a, b)"
