module GenProg.Test where
import Data.List
import System.Random
data Test = Test String deriving (Show)

mkUnaryTests :: (Random a, Show a, Show b) => (a -> b) -> (a, a) -> Int -> IO [Test]
mkUnaryTests f range n = do
  gen <- newStdGen
  return $ map (mkUnaryTest f) $ take n $ randomRs range gen

mkUnaryTest :: (Show a, Show b) => (a -> b) -> a -> Test
mkUnaryTest f i = Test $ "\\f -> f " ++ show i ++ " == " ++ show (f i)

showTests :: [Test] -> String
showTests tests = "[" ++ (intercalate "," $ map (\(Test s) -> s) tests) ++ "]"
