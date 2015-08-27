{-# LANGUAGE RecordWildCards #-}
module GenProg where

import Control.Monad.State

import Types.Value
import GenProg.Types
import GenProg.Mutations


type GenProgState = Population

mutate :: StateT GenProgState IO ()
mutate = do
  Population{..} <- get

  individuals <- liftIO $ (concat <$> mapM mutations individuals)

  liftIO $ mapM_ (putStrLn . showValue . func) individuals

  put (Population{..})

evaluate :: StateT GenProgState IO ([Either String Int])
evaluate = do
  pop <- get

  liftIO $ executePopulation pop

select :: [Either String Int] -> StateT GenProgState IO ()
select fits = do
  Population{..} <- get

  -- TODO This is where I left off 

  let indsWScore = filter (isRight.snd) $ zip individuals

  let sorted = sortOn snd indsWScore

  let count = length individuals `div` 2

  let bestWScores = drop (length sorted - count) sorted

  individuals <- return $ snd <$> bestWScores

  put (Population{..})

loop :: StateT GenProgState IO ()
loop = do
  -- The steps for genetic programming is mutate => evaluate => select

  mutate

  select =<< evaluate

  -- I will need to make an if statement to stop it, maybe...

runGenProg ind tests = do
  let population = Population [ind] tests
  evalStateT loop (population)
