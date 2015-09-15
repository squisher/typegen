{-# LANGUAGE RecordWildCards #-}
{-|
Module      : GenProg
Description : Functions for runing a genetic algorithm to generate code
Copyright   : (c) Jordan Medlock, 2015
                  University of New Mexico, 2015
License     : None
Maintainer  : medlock@unm.edu
Stability   : experimental
Portability : POSIX
-}
module GenProg (
  runGenProg
) where

import Control.Monad.State
import Data.Either
import Data.Maybe
import Data.List
import Test.QuickCheck

import Types.Value
import Types.Type (showType)
import GenProg.Types
import GenProg.Mutations
import GenProg.Execute
import GenProg.RandomElement


type GenProgState = Population


prettyPrintInd (ind,fit) = do
  putStr $ (showValue $ func ind) ++ " => "
  putStrLn $ show fit

liftEither (x, Right y) = Right (x, y)
liftEither (x, Left y) = Left (x, y)

mutate :: Int -> StateT GenProgState IO ()
mutate count = do
  Population{..} <- get

  inds <- liftIO $ concat <$> mapM mutations individuals

  inds <- case count > (length inds) of
    False -> liftIO $ take count <$> (getRandomElements 0.00130146 inds)
    True -> return inds

  individuals <- return $ individuals ++ inds
  liftIO $ do
    putStr "Number of new individuals: "
    print $ length individuals

  put (Population{..})

evaluate :: StateT GenProgState IO ([Either String Int])
evaluate = do
  pop <- get

  liftIO $ executePopulation pop

select :: Int -> [Either String Int] -> StateT GenProgState IO ()
select count fits = do
  Population{..} <- get


  let (baddies, indsWScore) = partitionEithers $ map liftEither $ zip individuals fits



  let sorted = sortOn (negate.snd) indsWScore

  let bestWScores = nub $ take count sorted

  liftIO $ do
    if True
      then do
        putStrLn "Printing Best Individuals"
        mapM_ prettyPrintInd bestWScores
      else do
        putStrLn "Printing Worst Individuals"
        mapM_ prettyPrintInd baddies

  individuals <- return $ fst <$> bestWScores

  put (Population{..})

loop :: StateT GenProgState IO ()
loop = do
  -- The steps for genetic programming is mutate => evaluate => select

  mutate 20

  select 20 =<< evaluate

  loop

-- | Runs the genetic program on the given 'Individual' and list of 'Test's
-- infinitely many times. 
runGenProg ind tests = do
  putStrLn "Running TypeGen"
  let population = Population [ind] tests
  evalStateT loop (population)
