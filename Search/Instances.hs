{-# LANGUAGE OverloadedStrings #-}
module Search.Instances (getInstances, Instance(..)) where

import           Control.Applicative
import           Data.Maybe
import           Data.Either
import           System.IO
import           System.Process
import           Types.TestFunction
import           Types.Value
import Data.List.Split hiding (startsWith)
import Data.List
import           Text.Parsec.Language
import           Text.Parsec.Prim
import qualified Text.Parsec.Token    as T
import Search.ParseInstances
import GHC
import GHC.Paths ( libdir )
import DynFlags
import Control.Applicative
import Outputable
import PprTyThing
import System.Environment

noInstances = ["String","IOError","Rational"]

-- | Returns a list of instances for a query
-- As far as I can tell it works for every class
-- it however does not work for every concrete type
-- doesnt work for lists, tuples, or functions, String, weird stuff
-- Works for applied types as long as its values are concrete
-- So: Maybe Int, Either String Bool, although I would recommend some post-checking
getInstances :: String -> IO [Instance]
getInstances s | s `elem` noInstances = return []
getInstances s = do
    strings <- findInstanceStrings s ["Prelude"]
    let list = parseInstances (unlines strings)
    either (\x -> putStrLn (s ++ " => " ++ (unlines strings) ++ "\n" ++ show x ++ "\n\n") >> return []) return list

process (Just (tyThing, fixity, clsInsts, famInsts)) = (map pprInstance clsInsts)

startsWith prefix string = take (length prefix) string == prefix

findInstanceStrings str modules = defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    (paths, things) <- runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        let dflags' = foldl xopt_set (dflags { hscTarget = HscInterpreted, ghcLink = LinkInMemory })
                            [Opt_Cpp, Opt_ImplicitPrelude, Opt_MagicHash]
        setSessionDynFlags dflags'
        setContext $ map (IIDecl . simpleImportDecl . mkModuleName) modules
        let paths = (importPaths dflags')
        unqual <- getPrintUnqual
        names <- parseName str
        things <- mapM (getInfo True) names
        let sdocs = concatMap process things
        let docs = map (flip runSDoc (initSDocContext dflags' defaultUserStyle)) sdocs
        return (paths, docs)
    return $ map show things
