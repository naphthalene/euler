divisors num = [x | x <- [1..(div num 2)], (mod num x == 0)]

amicable = [x | x <- [1..9999], let sumDivs = sum $ divisors x, let sumAgain = sum $ divisors sumDivs, sumAgain == x, sumAgain /= sumDivs]

answer = sum amicable