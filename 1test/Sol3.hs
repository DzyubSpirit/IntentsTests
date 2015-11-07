module Sol3 (resNumbers) where

import Funcs

resNumbers :: (Int, Int)
resNumbers = resNumbersN 3

resNumbersN :: Int -> (Int, Int)
resNumbersN n = 
    let maxBound = (10^n) - 1
        minBound = (10^(n-1)) + 1
        nums = [maxBound,(maxBound-1)..minBound]
        foldFunc el (edge, (mul1, mul2)) = 
            let max = findMaxPairBiggerThan (edge `div` el) n el
            in maybe (edge, (mul1, mul2)) (\mVal -> (mVal*el, (el, mVal))) max 
    in snd $ foldr foldFunc (0, (0, 0)) nums


-- Maybe you can think that it`s identical to sol1 solution
-- but laziness of haskell allow to write things like we 
-- calculate all numbers but in real we calculate  only numbers
-- which we will be used. We can see it looking at the speed
-- of main program switching 'import Sol1' to 'import Sol3'
-- and backwards.
findMaxPairBiggerThan :: Int -> Int -> Int -> Maybe Int
findMaxPairBiggerThan edge digCount secondNum =
    let maxBound = (10^digCount) - 1
        minBound = (10^(digCount - 1)) + 1
        numbers = [maxBound, (maxBound-1)..minBound]
        mults = map (*secondNum) numbers 
        num_mults = zip numbers mults
        isPolyndromN = isPalindrom 10 (digCount*2)
        safeHead [] = Nothing
        safeHead x = Just (head x)
    in fmap fst . safeHead . filter (isPolyndromN.snd) . takeWhile ((>= edge).fst) $ num_mults