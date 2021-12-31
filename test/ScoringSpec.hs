{-# LANGUAGE NoImplicitPrelude #-}
module ScoringSpec (spec) where

import Import
import Scoring
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  let zeros = undefined
  describe "score" $ do
    it "score zeros" $ score zeros `shouldBe` 0

    --it "overflow" $ plus2 maxBound `shouldBe` minBound + 1
    --prop "minus 2" $ \i -> plus2 i - 2 `shouldBe` i

