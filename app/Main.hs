module Main where

import NextLargest (digits, nextLargest, nextLargestRef)

import Test.QuickCheck
import Criterion.Main

prop_ref :: NonNegative Integer -> Bool
prop_ref (NonNegative x) = nextLargest x == Just (nextLargestRef x) || nextLargest x == Nothing

benchMap f = map (\x -> bench (show $ length $ digits x ) $ whnf f x )

inputValues = [ 12, 1234, 19000, 235467, 1648449, 16580449, 165800449, 1658000449, 98765432109876543210 ]

mainTest = defaultMain  [
    bgroup "ref"  ( benchMap nextLargestRef $ take 6 inputValues )
  , bgroup "fast" ( benchMap nextLargest    inputValues )
  ]

main :: IO ()
main = do
  quickCheck prop_ref
  mainTest
