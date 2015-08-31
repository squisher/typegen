{-# LANGUAGE RecordWildCards #-}
module GenProg where

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
  putStrLn $ showTypeStructure $ func ind

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

  let bestWScores = take count sorted

  liftIO $ do
    putStrLn "Printing Worst Individuals"
    mapM_ prettyPrintInd baddies
    -- mapM_ prettyPrintInd bestWScores

  individuals <- return $ fst <$> bestWScores

  put (Population{..})

loop :: StateT GenProgState IO ()
loop = do
  -- The steps for genetic programming is mutate => evaluate => select

  mutate 20

  select 20 =<< evaluate

  loop

runGenProg ind tests = do
  let population = Population [ind] tests
  evalStateT loop (population)
