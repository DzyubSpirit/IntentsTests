module Sol1 (resNumbers) where

import Funcs

resNumbers :: (Int, Int)
resNumbers = resNumbersN 3

resNumbersN :: Int -> (Int, Int)
resNumbersN n = snd. maximum . filter (isPalindrom 10 (n*2).fst) $ 
                    [((x*y), (x, y)) | 
                        x <- [minBound..maxBound], 
                        y <- [x..maxBound]
                    ]
        where minBound = (10^(n-1)) + 1
              maxBound = (10^n) - 1
