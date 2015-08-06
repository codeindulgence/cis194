data Peg = A | B | C deriving Show
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 from to _    = [(from, to)]
hanoi n from to temp = clearTop ++ moveBase ++ restoreTop
  where clearTop   = hanoi (n-1) from temp to
        moveBase   = [(from, to)]
        restoreTop = hanoi (n-1) temp to from

moveCount :: Integer -> Int
moveCount n = length $ hanoi n A B C
