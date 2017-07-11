toDigits :: Integer -> [Integer]
toDigits n
 | n < 10 = [n]
 | otherwise  = (mod n 10) : toDigits(div n 10)

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
 | n < 10 = [n]
 | otherwise  = (mod n 10) : toDigitsRev(div n 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = reverse $ doubleEverySecond $ reverse x
  where doubleEverySecond [] = []
        doubleEverySecond (x : []) = [x]
        doubleEverySecond (x : y : zs) =  x : (y * 2) : doubleEverySecond zs

sumDigits :: [Integer] -> Integer
sumDigits [ ] = 0
sumDigits (x:xs)
  | x >= 10 = sumDigits ((toDigits x ) ++ xs)
  | otherwise = x +sumDigits xs

validate :: Integer -> Bool

validate x = (sumDigits $ doubleEveryOther $ reverse $ toDigits x) `mod` 10 == 0
