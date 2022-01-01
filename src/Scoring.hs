{-# LANGUAGE NoImplicitPrelude #-}
module Scoring (Roll(..), Frame(..), Game(..), score) where


import Import

type Roll =  Int

data Frame = Frame {
              roll1 :: Roll, 
              roll2 :: Maybe Roll,
              roll3 :: Maybe Roll
            }  
            | Spare
            | Strike 


type Game =  [Frame]

score :: Game -> Int
score game | 10 == length game = score' game 0
score _ | otherwise = 0

score' :: Game -> Int -> Int
score' [] currentScore =  currentScore
score' (x: xs) currentScore = 
  let frameScore = scoreFrame x xs
      newScore = frameScore + currentScore
   in score' xs newScore
score' _ _ | otherwise = 0

scoreFrame :: Frame -> [Frame] -> Int
scoreFrame (Frame roll1 roll2 roll3) framesAfterRoll = 
  let roll2Score = case roll2 of
                     Just r2 -> r2
                     Nothing -> 0
      roll3Score = case roll3 of 
                     Just r3 -> r3 
                     Nothing -> 0
  in 
    roll1 + roll2Score + roll3Score
scoreFrame Spare framesAfterRoll = undefined
scoreFrame Strike framesAfterRoll = undefined

addNextRolls:: Game -> Int -> Int
addNextRolls = undefined
