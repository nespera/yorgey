toDigits :: Integer -> [Integer]
toDigits x 
  | x <= 0 = []
  | otherwise = map (\c -> read [c] :: Integer)(show x)

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse (toDigits x)

