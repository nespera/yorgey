toDigits :: Integer -> [Integer]
toDigits x 
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
