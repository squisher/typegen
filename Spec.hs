import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Types.Type
import Control.Monad.Trans.Maybe
import Types.ProcessTypes

testTypeSetup = 
  let a = (Polymorphic "a" (map Constraint []))
      b = (Polymorphic "b" (map Constraint []))
      list x = (Application (Concrete "[]") x)
  in (a, b, list)

main :: IO ()
main = hspec $ do
--  describe "Prelude.head" $ do
--    it "returns the first element of a list" $ do
--      head [23 ..] `shouldBe` (23 :: Int)

  describe "Test.ProcessTypes" $ do
    it "currying map" $ do
      let (a, b, list) = testTypeSetup
      let map' = (Function (Function a b) (Function (list a) (list b)))
      applic <- runMaybeT $ apply map' (Function a a)
      applic `shouldBe` Just (Function (Application (Concrete "[]") (Polymorphic "b" [])) (Application (Concrete "[]") (Polymorphic "b" [])))
      
    it "currying filter with correct function type" $ do
      let (a, b, list) = testTypeSetup
      let filter' = (Function (Function a (Concrete "Bool")) (Function (list a) (list a)))
      applic <- runMaybeT $ apply filter' (Function a (Concrete "Bool"))
      applic `shouldBe` Just (Function (Application (Concrete "[]") (Polymorphic "a" [])) (Application (Concrete "[]") (Polymorphic "a" [])))
      
      
    it "currying filter with incorrect function type" $ do
      let (a, b, list) = testTypeSetup
      let filter' = (Function (Function a (Concrete "Bool")) (Function (list a) (list a)))
      applic <- runMaybeT $ apply filter' (Function a a)
      applic `shouldBe` Nothing
