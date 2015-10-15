{-# LANGUAGE OverloadedStrings #-}

module Main where

import Search.ParseInstances
import Search.Instances
import Types.TestFunction
import Types.Type
import Types.Value
import Types.ProcessTypes
import Control.Applicative
import GenProg.Types
import GenProg.Test
import GenProg.Mutations
import GenProg.Execute
import Control.Monad
import System.Environment
import Control.Arrow
import Data.Either
import GenProg
import GenProg.RandomElement
import System.Random
import Control.Monad.Trans.Maybe

main = do
  setStdGen $ mkStdGen 0
  testGenProg

testGenProg = do
  let a x = (Polymorphic "a" (map Constraint x))
  let num = a ["Num","Eq"]
  let lit n = Atom num (show n)
  let ind = Individual "f :: (Eq a, Num a) => a -> a" ["x :: (Eq a, Num a) => a"] [] (lit 1)

  tests <- mkUnaryTests (\x -> x^2) (0::Int,10) 10

  runGenProg ind tests


testTypeStuff = do
  let a = (Polymorphic "a" (map Constraint []))
  let b = (Polymorphic "b" (map Constraint []))
  let list x = (Application (Concrete "[]") x)
  let map' = (Function (Function a b) (Function (list a) (list b)))
  let filter' = (Function (Function a (Concrete "Bool")) (Function (list a) (list a)))
  applic <- runMaybeT $ apply filter' (Function a a)
  print applic
  print $ showType <$> applic

testPrintPop :: Int -> IO ()
testPrintPop n = do
  let a x = (Polymorphic "a" (map Constraint x))
  let b x = (Polymorphic "b" (map Constraint x))
  let plusT = (Function (a ["Num"]) (Function (a ["Num"]) (a ["Num"])))
  let num = a ["Num"]
  let lit n = Atom num (show n)
  let plus = Atom plusT "(+)"
  let ind = Individual "f :: (Num a) => a -> a" ["x :: (Num a) -> a"] [] (lit 1)
  x <- mutations ind
  x <- concat <$> mapM mutations x
  x <- concat <$> mapM mutations x
  mapM_ (putStrLn . showIndividual) x
  -- tests <- mkUnaryTests (\x -> x^2) (0::Int,10) 10
  -- let pop = Population y tests
  -- mapM_ (putStrLn . showValue . func) y
  -- fits <- executePopulation pop
  -- mapM_ print $ zip [1..] fits

testRunPop :: IO ()
testRunPop = do
  let a x = (Polymorphic "a" (map Constraint x))
  let b x = (Polymorphic "b" (map Constraint x))
  let plusT = (Function (a ["Num"]) (Function (a ["Num"]) (a ["Num"])))
  let num = a ["Num"]
  let lit n = Atom num (show n)
  let plus = Atom plusT "(+)"
  let ind = Individual "f :: (Num a) => a -> a" ["x :: (Num a) => a"] [] (lit 1)
  putStrLn "Mutating origininal individual"
  x <- mutations ind
  -- mapM_ (putStrLn . showValue . func) x
  tests <- mkUnaryTests (\x -> x^2) (0::Int,10) 10
  let pop = Population x tests
  -- mapM_ (putStrLn . showValue . func) x
  putStrLn "Testing population"
  fits <- executePopulation pop
  mapM_ (print . ((either id show) *** (showValue . func))) $ zip fits x
  let newInds = map snd $ (filter (isRight.fst) $ zip fits x)
  putStrLn "Mutating new population"
  newX <- concat <$> mapM mutations newInds
  mapM_ (putStrLn . showValue . func) newX
  let newPop = Population newX tests
  fits <- executePopulation newPop
  putStrLn "Testing new population"
  mapM_ (print.(showIndividual***id)) $ zip x fits


testTests = do
  tests <- mkUnaryTests (\x -> x^2) (0::Int,10) 100
  putStrLn $ showTests tests

testInstances = do
  x <- getInstances "Integer"
  print x

testMutations = do
  let a x = (Polymorphic "a" (map Constraint x))
  let b x = (Polymorphic "b" (map Constraint x))
  let plusT = (Function (a ["Num"]) (Function (a ["Num"]) (a ["Num"])))
  let num = a ["Num"]
  let lit n = Atom num (show n)
  let plus = Atom plusT "(+)"
  let ind = Individual "f :: (Num a) => a -> a -> a" ["x :: (Num a) -> a","y :: (Num a) -> a"] [] (lit 1)
  x <- mutations ind
  mapM_ (printInd) x
  mapM_ ((mapM_ printInd)<=<mutations) x

printInd ind = do
  putStrLn $ showIndividual ind
  putStrLn ""
