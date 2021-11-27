module Solution (solution) where

solution :: Int
solution = head $ filter (\i -> mod i 7 == 0) onlyOneZeroList
    where
        onlyOneZeroList = [1..] >>= \i -> map (repeatOne i -) (reversePowerOf  (i - 1))
        repeatOne i = read $ replicate i '1'
        reversePowerOf i = reverse $ take i $ iterate (*10) 1
