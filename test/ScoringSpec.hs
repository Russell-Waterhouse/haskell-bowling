{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ScoringSpec (spec) where

import Import
import Scoring
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  let zeroRoll :: Roll = 0
      zeroFrame :: Frame = Frame zeroRoll (Just zeroRoll) Nothing
      zeroGame :: Game = makeGameOfFrame zeroFrame (Frame zeroRoll (Just zeroRoll) (Just zeroRoll))
      oneFrame :: Frame = Frame (1) (Just 1) Nothing
      onesGame :: Game = makeGameOfFrame oneFrame oneFrame
      strikeFrame :: Frame = Strike
      perfectGame :: Game = makeGameOfFrame strikeFrame strikeFrame
  describe "score" $ do
    it "score invalid Game" $ score [] `shouldBe` 0
    it "score zeros" $ score zeroGame `shouldBe` 0
    it "score 1's" $ score onesGame `shouldBe` 20
   -- it "score perfect game" $ score perfectGame `shouldBe` 300

    --it "overflow" $ plus2 maxBound `shouldBe` minBound + 1
    --prop "minus 2" $ \i -> plus2 i - 2 `shouldBe` i

makeGameOfFrame :: Frame -> Frame -> Game
makeGameOfFrame frame lastFrame = 
  [frame, frame, frame, frame, frame, frame, frame, frame, frame, lastFrame]
