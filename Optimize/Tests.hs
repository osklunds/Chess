
{-# LANGUAGE TemplateHaskell #-}

module Optimize.Tests where

import Test.QuickCheck
import System.Random

import Optimize.MiniMax


--------------------------------------------------------------------------------
-- Fixed
--------------------------------------------------------------------------------

prop_fixed1 :: Bool
prop_fixed1 = optimize genSts evalSt 0 initSt == initSt &&
              optimize genSts evalSt 1 initSt == 3      &&
              optimize genSts evalSt 2 initSt == 2      &&
              optimize genSts evalSt 3 initSt == 1
  where
    initSt = 0

    -- My moves, first turn
    genSts 0 = [1,2,3]

    -- Opponent's moves, second turn
    genSts 1 = [11,12]
    genSts 2 = [21,22,23]
    genSts 3 = [31]

    -- My moves, third turn
    genSts 11 = [111]
    genSts 12 = [121,122]
    
    genSts 21 = [211]
    genSts 22 = [221,222,223]
    genSts 23 = [231,232]
    
    genSts 31 = [311,312,313]

    -- Opponent's turn, fourth turn
    genSts 111 = [1111,1112]
    
    genSts 121 = [1211]
    genSts 122 = [1221,1222]
    
    genSts 211 = [2111,2112,2113,2114]
    
    genSts 221 = [2211]
    genSts 222 = [2221,2222]
    genSts 223 = [2231]
    
    genSts 231 = [2311]
    genSts 232 = [2321,2322]
    
    genSts 311 = [3111,3112]
    genSts 312 = [3121,3122,3123]
    genSts 313 = [3131]

    -- No move
    evalSt 0 = 5

    -- My moves, first turn
    evalSt 1 = 100
    evalSt 2 = 200
    evalSt 3 = 300 -- Current best move

    -- Opponent's moves, second turn
    evalSt 11 = 050 -- Bad move, made it good for me
    evalSt 12 = 350 -- Good move, made it bad for me
      -- Min is 050
    evalSt 21 = 250
    evalSt 22 = 200
    evalSt 23 = 150
      -- Min is 150
    evalSt 31 = 100 
      -- Min is 100
    -- So if the opponent takes the move that makes it worst for me,
    -- I should make sure that the worst move is still as good as
    -- possible for me, meaning I should force the opponent to choose 150
    -- as the min, i.e. I should take move 2.

    -- My moves, third turn
    evalSt 111 = 300 -- The best, but this won't happen, because if I chose
                     -- move 1 in turn 1, the opponent will choose move 2 in
                     -- turn 2, so I'll never have a chance to choose move 1 in
                     -- turn 3.
    evalSt 121 = 120
    evalSt 122 = 160 -- Will be chosen, because of all moves that the opponent
                     -- allowed me to do, this is the best.
    
    evalSt 211 = 300 -- Same here. Move 3 will be chosen if I chose move 3
                     -- in turn 1.
    evalSt 221 = 310
    evalSt 222 = 320
    evalSt 223 = 330
    evalSt 231 = 100
    evalSt 232 = 110
    
    evalSt 311 = 130
    evalSt 312 = 140
    evalSt 313 = 150

    -- Opponent's moves, fourth turn
    evalSt 1111 = 101
    evalSt 1112 = 102

    evalSt 1211 = 103
    evalSt 1221 = 104
    evalSt 1222 = 105
    
    evalSt 2111 = 106
    evalSt 2112 = 107
    evalSt 2113 = 108
    evalSt 2114 = 109

    evalSt 2211 = 110
    evalSt 2221 = 111
    evalSt 2222 = 112
    evalSt 2231 = 113
    
    evalSt 2311 = 114
    evalSt 2321 = 115
    evalSt 2322 = 116
    
    evalSt 3111 = 117
    evalSt 3112 = 118
    evalSt 3121 = 119
    evalSt 3122 = 120
    evalSt 3123 = 121
    evalSt 3131 = 122




return []
runTests = $quickCheckAll


--------------------------------------------------------------------------------
-- Arbitrary
--------------------------------------------------------------------------------

