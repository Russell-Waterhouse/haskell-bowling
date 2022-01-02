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
      zeroGame :: Game = makeGameOfFrame zeroFrame (Frame zeroRoll (Just zeroRoll) $ Just zeroRoll)
      oneFrame :: Frame = Frame (1) (Just 1) Nothing
      onesGame :: Game = makeGameOfFrame oneFrame oneFrame
      fiveFrame :: Frame = Frame 5 (Just 5) Nothing
      fiveFrame10 :: Frame = Frame 5 (Just 5) $ Just 5
      fivesGame :: Game = makeGameOfFrame fiveFrame fiveFrame10
      strikeFrame :: Frame = Frame 10 Nothing Nothing
      strikeFrame10 :: Frame = Frame 10 (Just 10) $ Just 10
      perfectGame :: Game = makeGameOfFrame strikeFrame strikeFrame10
  describe "score" $ do
    it "score invalid Game" $ score [] `shouldBe` 0
    it "score zeros" $ score zeroGame `shouldBe` 0
    it "score 1's" $ score onesGame `shouldBe` 20
    it "score all 5's" $ score fivesGame `shouldBe` 150
    it "score perfect game" $ score perfectGame `shouldBe` 300


makeGameOfFrame :: Frame -> Frame -> Game
makeGameOfFrame frame lastFrame = 
  [frame, frame, frame, frame, frame, frame, frame, frame, frame, lastFrame]
