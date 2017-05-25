module NextLargest ( digits, nextLargest, nextLargestRef ) where

import Data.List (span, sort, permutations)

-- 'Reference'  implementation: brute force
nextLargestRef :: Integer -> Integer
nextLargestRef x = head $ filter (>x) $ map read $ sort $ permutations $ show x

-- Wrap the packing and unpacking of the Integer to lists of digits
nextLargest :: Integer -> Maybe Integer
nextLargest = number . nextLargest' . digits

digits :: Integer -> [Int]
digits = map ( read . (:[]) ) . show

number :: Maybe [Int] -> Maybe Integer
number Nothing  = Nothing
number (Just x) = Just (read $ map ( head . show ) x)

-- Monad because I can
nextLargest' :: [Int] -> Maybe [Int]
nextLargest' l = do
  partitioned <- partitionDigits (reverse l, [])
  sorted      <- sortDigits partitioned
  swapped     <- swapDigits sorted
  makeWhole swapped

-- Split the reversed list of digits into a monotonically increasing part and the rest
partitionDigits :: ([Int], [Int]) -> Maybe ([Int], [Int])
partitionDigits ([],_)     = Nothing
partitionDigits (x:xs,[])  = partitionDigits (xs, x:[])
partitionDigits (x:xs,y:ys)
  | x < y                  = Just (x:xs,y:ys)
  | otherwise              = partitionDigits (xs, x:y:ys)

-- Split the second part of the tuple into a list of elements smaller than or equal to
-- the first element (i.e. last digit) of the (reversed) first part and the rest
sortDigits :: ([Int], [Int]) -> Maybe ([Int], ([Int], [Int]))
sortDigits ([],_) = Nothing
sortDigits (x,y)  = Just (x, span (<= head x) $ reverse y) -- sort y == reverse y in this case, because we had an ascending series from the back of the list

-- Swap the first element of the (reversed) first part and the first element of the last part
swapDigits :: ([Int], ([Int], [Int])) -> Maybe ([Int], [Int], [Int])
swapDigits ([],(_,_ ))     = Nothing
swapDigits (_ ,(_,[]))     = Nothing
swapDigits (x:xs,(y,z:zs)) = Just (z:xs,y,x:zs)

-- Put the array together again, reversing the first part
makeWhole :: ([Int], [Int], [Int]) -> Maybe [Int]
makeWhole (x,y,z) = Just (reverse x ++ y ++ z)
