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
  let command = (shell $ "echo \":i "++s++"\" | ghci") { std_out = CreatePipe
                                                       }
  (_, mhandle, _, _) <- createProcess command
  case mhandle of
    (Just hout) -> do
      string <- hGetContents hout
      let newString = "instance" ++ (intercalate "instance" $ tail $ splitOn "instance" string)
      let str = head $ splitOn "\955" newString
      let newString' = if startsWith "data" str
                        then unlines (tail (lines str))
                        else str
      let list = parseInstances newString'
      either (\x -> putStrLn (s ++ " => " ++ show newString ++ show x) >> return []) return list
    Nothing -> return []

startsWith prefix string = take (length prefix) string == prefix
