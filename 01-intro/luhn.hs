toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits x
  | x > 0 = toDigits tail ++ [last]
  | otherwise = []
  where last = mod x 10
        tail = div x 10

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse $ f $ reverse xs
  where f []       = []
        f [x]      = [x]
        f (x:y:ys) = x : 2*y : f ys

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits
