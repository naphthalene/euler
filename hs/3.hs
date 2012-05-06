--pfactors :: Integer -> [Integer]
pfactors 0 = []
pfactors 1 = []
pfactors n = [x | x <- [2..n], x < (ceiling . sqrt $ fromIntegral n), (n `mod` x) == 0]

bigNum = 600851475143

answer = last $ pfactors bigNum