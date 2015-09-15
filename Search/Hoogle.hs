{-|
Module      : Search.Hoogle
Description : This provides a function that searches hoogle for a type signature
Copyright   : (c) George Stelle, 2014
                  Jordan Medlock, 2015
Maintainer  : medlock@unm.edu
Stability   : experimental
Portability : POSIX

-}
{-# LANGUAGE TupleSections #-}
module Search.Hoogle (hoogle) where

import Hoogle
import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe
import Types.TestFunction
import Types.Type
import Types.ProcessTypes
import Types.Value
import System.IO.Unsafe
import System.IO
import Data.IORef
import qualified Data.Map.Lazy as Map
import Control.Arrow
-- import Types.Values as V

readTuple :: (Read a, Read b) => String -> (a, b)
readTuple = (read *** read) . break (=='\t')

memoize :: (Ord a) => (a -> IO b) -> IO (a -> IO b)
memoize f = do
  memo <- newIORef Map.empty
  return $ \s -> do
    m <- readIORef memo
    case Map.lookup s m of
      Nothing -> do
        x <- f s
        writeIORef memo $ Map.insert s x m
        return x
      (Just x) -> return x

fileMemoize :: (Show a, Read a, Show b, Read b, Eq a) => String -> (a -> IO b) -> a -> IO b
fileMemoize fname f s = do
  rH <- openFile fname ReadMode
  table <- map readTuple <$> lines <$> hGetContents rH
  case lookup s table of
    Nothing -> do
      x <- f s
      hClose rH
      wH <- openFile fname AppendMode
      hPutStrLn wH $ show s ++ "\t" ++ show x
      return x
    (Just x) -> return x

-- | Runs a hoogle search on the given type and returns a list of functions that
-- __/might/__ be applicable to that type.
hoogle :: Type -> IO [TestFunction]
hoogle = uhoogle

-- | Runs a hoogle search on the given type string and returns a list of scores
-- attached to results
uhoogle :: Type -> IO [TestFunction]
uhoogle inType = do
  let s = showType inType
  -- putStrLn ("Searching for: " ++ s)
  db <- loadDatabase "./default.hoo"
  let query = parseQuery Haskell $ "::" ++ s
  -- either (const $ putStrLn $ "::" ++ s) (const (return ())) query
  let value = either (error.show) (search db . queryExact (Nothing)) query
  let hoogleRes = processHoogle value

  filteredValues <- filterM (visibleFilter inType) hoogleRes
  return filteredValues

processHoogle :: [(Score, Result)] -> [TestFunction]
processHoogle = nub.map snd.filter goodEnough.liftAllToTestFunction

liftAllToTestFunction :: [(a, Result)] -> [(a, TestFunction)]
liftAllToTestFunction = catMaybes.map (\(x,y) -> (x,) <$> resultToTestFunction y)

-- | Maybe this could have been done with applicatives but I couldnt figure it out
-- HAHA applicatibes and tuple sections!!
-- liftMaybe :: (a, Maybe b) -> Maybe (a, b)
-- liftMaybe (x,Just y) = Just (x,y)
-- liftMaybe (_,Nothing) = Nothing


visibleFilter inType x = do
  let left = typ (funcValue x)
  res <- left `usableAs` inType
  return res

-- | Tests if a hoogle result is good enough to compile and test
goodEnough :: (Score, TestFunction) -> Bool
goodEnough (Score _ ts _, r) = (Just "Prelude") == modName r
                           -- && all (`elem` [CostUnrestrict, CostInstanceAdd]) ts


-- | Takes a Hoogle result and turns it into a TestFunction
resultToTestFunction :: Result -> Maybe TestFunction
resultToTestFunction result@(Result _ t _) = (Just.(TestFunction (Just mod'))) =<< a
  where mod' = (snd.last.snd.head.locations) result
        a = case parseTestFunction $ showTagText t of
          (Right (TestFunction _ a')) -> Just a'
          (Left _) -> Nothing -- error $ "could not parse " ++ show t ++ " got error: " ++ show a

-- | Removes the parentheses of a string
-- fixInfix :: String -> String
-- fixInfix = delete '(' . delete ')'

main = print =<< hoogle (Polymorphic "a" [])
