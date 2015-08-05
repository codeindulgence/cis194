toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits x
  | x > 0 = toDigits tail ++ [last]
  | otherwise = []
  where last = mod x 10
        tail = div x 10
