module Sol1 (resNumbers) where

import Funcs

resNumbers :: (Int, Int)
resNumbers = snd. maximum . filter (isPalindrom6.fst) $ [((x*y), (x, y)) | x <- [101..999], y <- [x..999]]