pfactors :: Integer -> [Integer]
pfactors 0 = []
pfactors 1 = []
pfactors n = [x | x <- [2..n], x < (ceiling sqrt n)]

                             
                             
                             
bigNum = 600851475143

answer = tail $ pfactors bigNum