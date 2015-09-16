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

-- histogram [1,1,1,5] ==
--  *
--  *
--  *   *
-- ==========
-- 0123456789

-- histogram [1,4,5,4,6,6,3,4,2,4,9] ==
--     *
--     *
--     * *
--  ******  *
-- ==========
-- 0123456789

histogram :: [Integer] -> String
histogram xs = unlines (map (row counts) [most, (most-1)..1]) ++
               "==========\n" ++ ['0'..'9']
  where counts = map (\x -> length $ filter (x==) xs) [0..9]
        most = maximum counts
        row xs n = [if x >= n then '*' else ' ' | x <- xs]
