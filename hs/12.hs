import Data.List

tris = [div (x * (x + 1)) 2 | x <- [1..]]

test `isFactor` num = (num `mod` test) == 0

--countDivisors :: (Integral a) => a -> a -> a -> [(a, a)] -> a
countDivisors num test acc cache 
  | test == 1 = acc
  | otherwise = if test `elem` (map fst cache)
                  then acc + (snd . head $ filter ((==test) . fst) cache)
                  else countDivisors num (pred test) (acc + (if (isFactor num test)
                                                               then 1 else 0)) cache
getPrimeFactors num
  | num == 1  = []
  | otherwise = numDivisor:(getPrimeFactors numRemainder)
      where numDivisor = head [x | x <- [2..num], x `isFactor` num]
            numRemainder = div num numDivisor

numDivisors = foldr (\x acc -> acc * (succ (length x))) 1 . group . sort . getPrimeFactors

-- = [x | x <- [1..(div num 2)], (mod num x == 0)]

answer = head [x | x <- tris, numDivisors x > 500] 

-- store each calculated triangle number and its number of factors as
-- a tuple such as:
-- [(3, 1), (6, 4)]

-- pass this to the divisors function as a sort of cache and define
-- divisors recursively 
