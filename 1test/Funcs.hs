module Funcs(isPalindrom6) where

isPalindrom6 :: Int -> Bool
isPalindrom6 = isPalindrom 10 6

isPalindrom :: Int -> Int -> Int -> Bool
isPalindrom osi digCount ch = 
     let accFunc osi (acc, r, d) = let (z, r') = divMod r d
                           in (z:acc, r', d `div` osi) 
         fst3 (x, _, _) = x
         zes = fst3 . flip (!!) digCount . iterate (accFunc osi) $ ([], ch, osi^(digCount-1))
         zesRev = reverse zes
     in zes == zesRev