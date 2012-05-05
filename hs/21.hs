divisors num = [x | x <- [1..(div num 2)], (mod num x == 0)]

amicable = [x | x <- [1..9999], (sum . divisors . sum $ divisors x) == x]
-- generates [1,6,28,220,284,496,1184,1210,2620,2924,5020,5564,6232,6368,8128]

-- TODO: check for amicable pairs where a /= b