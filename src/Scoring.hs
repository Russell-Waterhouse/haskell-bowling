{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Scoring (Roll, Game, Frame(..), score) where


import           Import

strike :: Int
strike = 10
spare :: Int
spare = 10
numFramesInGame :: Int
numFramesInGame = 10

type Roll =  Int

data Frame = Frame {
              roll1 :: Roll,
              roll2 :: Maybe Roll,
              roll3 :: Maybe Roll
            }


type Game =  [Frame]

score :: Game -> Int
score game | numFramesInGame == length game = score' game 0
score _ | otherwise = 0

score' :: Game -> Int -> Int
score' [] currentScore =  currentScore
score' (x: xs) currentScore =
  let frameScore = scoreFrame x xs
      newScore = frameScore + currentScore
   in score' xs newScore

scoreFrame :: Frame -> [Frame] -> Int
-- strike
scoreFrame (Frame r1 r2 r3) framesAfterRoll 
  | r1 == strike =
    strike + (addNextRolls (Frame strike r2 r3) framesAfterRoll 2)
-- spare
scoreFrame (Frame r1 (Just r2) r3) framesAfterRoll
  | r1 + r2 == spare =
    spare + (addNextRolls (Frame r1 (Just r2) r3) framesAfterRoll 1)
-- otherwise
scoreFrame (Frame r1 (Just r2) r3) _
  | otherwise =
    let roll3Score :: Int = case r3 of
                       Just roll3points -> roll3points
                       Nothing          -> 0
    in
      r1 + r2 + roll3Score
-- all valid cases have been defined, this covers invalid cases
scoreFrame _ _ = undefined

addNextRolls:: Frame -> Game -> Int -> Int
-- add no next rolls
addNextRolls _ _ 0 = 0
-- 2 strikes in frame 10
addNextRolls (Frame r1 (Just r2) (Just r3)) [] 2 
  | r1 == strike && r2 == strike = strike +  r3
-- 1 strike in frame 10
addNextRolls (Frame  r1 (Just r2) (Just r3)) [] 2 
  | r1 == strike = r2 + r3
-- a spare in frame 10
addNextRolls (Frame r1 (Just r2) (Just r3)) [] 1
  | r1 + r2 == spare = r3
-- current frame was a spare and not frame 10
addNextRolls _ (nextFrame: _) 1 =
  (roll1 nextFrame)
-- current frame was a strike and not frame 10
addNextRolls currentFrame (nextFrame: otherFrames) 2 =
  let
      r1 = roll1 nextFrame
      r2 = roll2 nextFrame
      firstRollScore :: Int = r1
      secondRollScore :: Int = case r2 of
                     Just roll2Score -> roll2Score
                     Nothing         -> addNextRolls currentFrame otherFrames 1
  in
    firstRollScore + secondRollScore
-- all other cases are invalid
addNextRolls _ _ _ = undefined

