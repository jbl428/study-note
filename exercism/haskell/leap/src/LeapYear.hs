module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear year = and [year `mod` 4 == 0, or [ year `mod` 100 /= 0, year `mod` 400 == 0]]

