lastDigit :: Integer -> Integer
lastDigit n
  | n <=0 = 0
  | otherwise = n `mod` 10

dropLastDigit :: Integer -> Integer
dropLastDigit n
  | n < 10 = 0
  | otherwise = n `div` 10

toDigits :: Integer -> [Integer]
toDigits x
  | x < 0 = []
  | x < 10 = [x]
  | otherwise = (toDigits (dropLastDigit x)) ++ [lastDigit x]

toDigits2 :: Integer -> [Integer]
toDigits2 x 
  | x <= 0 = []
  | otherwise = map (\c -> read [c] :: Integer)(show x)

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse (toDigits x)

doDouble :: [Integer] -> [Integer]
doDouble [] = []
doDouble (x:xs) = (2*x) : (dontDouble xs)

dontDouble :: [Integer] -> [Integer]
dontDouble [] = []
dontDouble (x:xs) = x : (doDouble xs)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = reverse (dontDouble (reverse x))

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum (toDigits x) + (sumDigits xs)

validate :: Integer -> Bool
validate n = (sumDigits (doubleEveryOther (toDigits n))) `mod` 10 == 0 

