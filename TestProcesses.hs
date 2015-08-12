import GHC
import GHC.Paths ( libdir )
import DynFlags
import Control.Applicative
import Outputable
import Control.Monad.IO.Class
import PprTyThing
import System.Environment
 
process (Just (tyThing, fixity, clsInsts, famInsts)) = (map pprInstance clsInsts)

getInstances str modules = defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
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

main = do
    args <- getArgs
    mapM_ putStrLn =<< getInstances (head args) ["Prelude"]
