-- Collatz problem
-- n -> n/2     =(n is even)
-- n -> 3n + 1  =(n is odd)

-- Can exclude all powers of 2

powersOfTwo = [2^x | x <- [1..19]]

startingNums = [x | x <- [1..999999], not $ x `elem` powersOfTwo]

collatz' n = collatz n []

collatz 1 _ = 1
collatz n cache
  | inCache   = snd $ head match
  | even n    = 1 + (collatz (div n 2) cache)
  | otherwise = 1 + ( collatz ((3 * n) + 1) cache)
    where match = filter (\(x, cx) -> x == n) cache
          inCache = not . null $ match
                
                    
simpleCollatz 1 = 1
simpleCollatz n   
  | even n = 1 + (simpleCollatz (div n 2))
  | otherwise = 1 + (simpleCollatz ((3 * n) + 1))
             
                    
scanCollatz' = scanCollatz (0,0) []


scanCollatz resTuple _ [] = resTuple
scanCollatz (prevn, prevCofn) cache (n:ns) 
  | cofn > prevCofn = scanCollatz (n, cofn) ((n,cofn):cache) ns
  | otherwise       = scanCollatz (prevn, prevCofn) ((n,cofn):cache) ns
    where cofn = collatz n cache
            
            
-------------------------------
          
scanSimpleCollatz resTuple [] = resTuple
scanSimpleCollatz (prevn, prevCofn) (n:ns)
  | cofn > prevCofn = scanSimpleCollatz (n, cofn) ns
  | otherwise       = scanSimpleCollatz (prevn, prevCofn) ns
    where cofn = simpleCollatz n
          
scanSimpleCollatz' = scanSimpleCollatz (0,0)



answer = scanCollatz' startingNums