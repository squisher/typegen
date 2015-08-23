{-# LANGUAGE ScopedTypeVariables #-}
import GHC
import GHC.Paths ( libdir )
import DynFlags
import Outputable
import Control.Applicative
import Control.Exception
import PprTyThing
import System.Environment
import Unsafe.Coerce
import GhcMonad (liftIO)
import Data.Either.Extra
import System.Posix.Signals

runFile filename = defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        setSessionDynFlags $ dflags { hscTarget = HscInterpreted
                                    , ghcLink   = LinkInMemory
                                    }
        setTargets =<< sequence [ guessTarget filename Nothing ]
        load LoadAllTargets
        setContext [ IIModule $ mkModuleName "Test"
                   , IIDecl $ simpleImportDecl $ mkModuleName "Control.Monad" ]
        act <- unsafeCoerce <$> compileExpr "forM_ [1,2,test] print"
        liftIO act


runString string = try $ do
    runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        setSessionDynFlags $ dflags { hscTarget = HscInterpreted
                                    , ghcLink   = LinkInMemory
                                    }
        setContext [ IIDecl $ simpleImportDecl $ mkModuleName "Control.Monad"
                   , IIDecl $ simpleImportDecl $ mkModuleName "Prelude" ]
        act <- unsafeCoerce <$> compileExpr string
        return  act


main = do
    installHandler busError Ignore Nothing -- (Catch (throwIO $ NoMethodError "You screwed up the type")) Nothing
    runFile "test.hs" 
    runFile "test2.hs" 
    f1 <- runString "let f x = undefined in f" :: IO (Either SomeException (Int -> Int))
    f2 <- runString "let f x = x + 10 in f" :: IO (Either SomeException (Int -> Int))
    f3 <- runString "let f x = x + 10 in f" :: IO (Either SomeException (Bool -> Bool)) 
    putStrLn "First test"
    print $ (fromRight f2) 10
    putStrLn "Second"
    print =<< ((try $ return $ fromRight f3 True) :: IO (Either SomeException Bool))
    putStrLn "Third"
