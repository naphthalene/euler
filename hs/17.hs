
-- every entry in numDict is a tuple (n, l) where n is a number with a unique
-- spelling, and l is the number of letters in it
numDict = [(1, 3), (2, 3), (3, 5), (4, 4), (5, 4), (6, 3), (7, 5), (8, 5)
          ,(9, 4), (10,3), (11,6), (12,6), (13,8), (14,8), (15,7), (16,7)
          ,(17,9), (18,8), (19,8), (20,6), (30,6), (40,5), (50,5), (60,5)
          ,(70,7), (80,6), (90,6)]
          
revdigs 0 = []
revdigs x = (x `mod` 10):(revdigs (x `div` 10))

digs 0 = []
digs x = (digs (x `div` 10)) ++ [(x `mod` 10)]


countLetters num
  | num < 1 = error ""
  | not . null $ match = snd match
    where match = head $ dropWhile (\(n, l) -> n /= num)
