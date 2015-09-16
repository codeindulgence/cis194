module Golf where

-- skips "ABCD"       == ["ABCD", "BD", "C", "D"]
-- skips "hello!"     == ["hello!", "el!", "l!", "l", "o", "!"]
-- skips "123456"     == ["123456", "246", "36", "4", "5", "6"]
-- skips [1]          == [[1]]
-- skips [True,False] == [[True,False], [False]]
-- skips []           == []

skips :: [a] -> [[a]]
skips xs = [skips' (n-1) xs | n <- [1..(length xs)]]
  where skips' n xs = case tail' of
          [] -> []
          _  -> head tail' : skips' n (tail tail')
          where tail' = drop n xs

-- localMaxima [2,9,5,6,1] == [9,6]
-- localMaxima [2,3,4,1,5] == [4]
-- localMaxima [1,2,3,4,5] == []

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs)
  | y > x && y > z = y : localMaxima (y:z:xs)
  | otherwise      = localMaxima (y:z:xs)
localMaxima _ = []
