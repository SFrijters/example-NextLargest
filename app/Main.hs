module Main where

import NextLargest (digits, nextLargest, nextLargestRef, nextLargestRef')

import Test.QuickCheck
import Criterion.Main

prop_ref :: NonNegative Integer -> Bool
prop_ref (NonNegative x) = nextLargest x == Just (nextLargestRef x) || nextLargest x == Nothing

resultsMap :: (Integer -> Maybe Integer) -> [ (Integer, Integer) ] -> [ Bool ]
resultsMap f = map (\x -> f (fst x) == Just (snd x))

benchMap f = map (\x -> bench (show $ length $ digits (fst x) ) $ whnf f (fst x))

inputValues = [ (12, 21)
              , (1234, 1243)
              , (1243, 1324)
              , (19000, 90001)
              , (234765, 235467)
              , (292761, 296127)
              , (1654984, 1658449)
              , (16549840, 16580449)
              , (165498400, 165800449)
              , (1654984000, 1658000449)
              , (98765432109876543210, 98765432110023456789)
             ]

perfMain = defaultMain
  [ bgroup "ref"  ( benchMap nextLargestRef  $ filter (\x -> length (digits $ fst x) < 9) inputValues )
  , bgroup "ref'" ( benchMap nextLargestRef' $ filter (\x -> length (digits $ fst x) < 9) inputValues )
  , bgroup "fast" ( benchMap nextLargest                                                  inputValues )
  ]

main :: IO ()
main = do
  putStrLn $ show $ all (==True) $ resultsMap nextLargest inputValues
  quickCheck prop_ref
  perfMain
