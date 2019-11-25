import Data.List

main = do
  n <- readLn :: IO Integer
  putStrLn $ unwords . map show $ primeFactors n
  -- k <- readLn :: IO Int
  -- putStrLn $ show $ head $ reverse $ take k $ primes


-- gcd :: Integer -> Integer -> Integer
-- gcd a 0 = a
-- gcd a b
--     | a < b     = gcd b a
--     | otherwise = gcd b (mod a b)

sqrtInt :: Integer -> Integer
sqrtInt x = floor $ (+ 0.01) $ sqrt $ fromIntegral x

sieve :: [Integer] -> [Integer] -- too slow
sieve (p:xs) = p : sieve [ x | x <- xs, mod x p /= 0 ]
-- primes = sieve [2..]

primes :: [Integer]
primes = 2 : [ n | n <- [3..], head (primeFactors n) == n ]

primeFactors :: Integer -> [Integer]
primeFactors n = factors n primes
    where factors n ps@(p:qs)
              | n < 2        = []
              | n < p^2      = [n]
              | mod n p == 0 = p : factors (div n p) ps
              | otherwise    = factors n qs

isPrime :: Integer -> Bool
isPrime n = head (primeFactors n) == n
