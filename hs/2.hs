import Data.List
import Data.Bits
-- Naive fib
fib x
  | x == 0          = 0
  | x == 1          = 1
  | otherwise       = fib (x - 1) + fib (x - 2)
                      
-- Tail-recursive fib -> don't know how to write this yet :c
--fib' x
--  | x <  0          = error "negative index requested"
--  | x == 0          = 0
--  | x == 1          = 1
--  | otherwise       = fibt x 1

-- from http://www.haskell.org/haskellwiki/The_Fibonacci_sequence

 
fib1 n = snd . foldl fib' (1, 0) . map (toEnum . fromIntegral) $ unfoldl divs n
    where
        unfoldl f x = case f x of
                Nothing     -> []
                Just (u, v) -> unfoldl f v ++ [u]
 
        divs 0 = Nothing
        divs k = Just (uncurry (flip (,)) (k `divMod` 2))
 
        fib' (f, g) p
            | p         = (f*(f+2*g), f^2 + g^2)
            | otherwise = (f^2+g^2,   g*(2*f-g))
                          
 
fib' :: Int -> Integer
fib' n = snd . foldl' fib'' (1, 0) . dropWhile not $
            [testBit n k | k <- let s = bitSize n in [s-1,s-2..0]]
    where
        fib'' (f, g) p
            | p         = (f*(f+2*g), ss)
            | otherwise = (ss, g*(2*f-g))
            where ss = f*f+g*g

-- at this point x >= 2
-- acc will store the current n - 1 value
--fibt :: (Int a) => a -> a -> a
--fibt n acc = fibt (n - 2)

useFib = fib'
                      
answer = sum (takeWhile (<=4000000) [fibOfX | x <- [1..], let fibOfX = useFib x, even fibOfX])
