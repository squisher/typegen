{-|
Module      : GenProg.Evaluate
Description : This module acts the same as the eval function
Copyright   : (c) Jordan Medlock, 2015
                  University of New Mexico, 2015
License     : None
Maintainer  : medlock@unm.edu
Stability   : experimental
Portability : POSIX

This module runs a string in Haskell and returns its value

I dont think this is used in the code anywhere, I think it was made to see if
it was possible.  It is possible with a lot of caveats which makes it /maybe/
unusable in this context.
-}
module GenProg.Evaluate (
  runString
) where

import GHC
import GHC.Paths ( libdir )
import DynFlags
import Control.Applicative
import Outputable
import PprTyThing
import System.Environment
import Unsafe.Coerce
import GhcMonad (liftIO)
import Control.Exception

ex :: SomeException -> SomeException
ex = id
-- | Takes a haskell string and a list of modules to import and compiles and executes
-- the string after bringing the modules into the current context.  It seems to always work...
-- I have not found any issues with it so far.  Make sure you add the correct type declaration
-- to the output so that it coerces to the correct type.  Ok nevermind I found an issue, make
-- sure that you cast it to the correct type, because there is no way to catch if you failed.
-- I would fix this by giving the value in the string a type definition so that it fails before
-- you try to evaluate its value
runString :: String -> [String] -> IO (Either String a)
runString string modules = handle (return . Left . show . ex) $ Right <$> do
    runGhc (Just libdir) $ do -- We need a GHC environment
        dflags <- getSessionDynFlags
        setSessionDynFlags $ dflags { hscTarget = HscInterpreted
                                    , ghcLink   = LinkInMemory
                                    } -- I am not sure why these flags are set, but they work
        setContext [ IIDecl $ simpleImportDecl $ mkModuleName "Control.Monad"
                   , IIDecl $ simpleImportDecl $ mkModuleName "Prelude" ]
                   -- TODO oops didnt implement modules
        act <- unsafeCoerce <$> compileExpr string
        return act
