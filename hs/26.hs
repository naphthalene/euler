digs 0 = []
digs x = (digs (x `div` 10)) ++ [(x `mod` 10)]

mag = length . digs

getCycle _ 0 acc = acc
getCycle 0 _ acc = acc
getCycle n d acc 
  = if modRem == n
      then divRes:acc
      else getCycle modRem d (divRes:acc)
        where nMag = n * (10  ^ (mag d))
              modRem = nMag `mod` d
              divRes = nMag `div` d