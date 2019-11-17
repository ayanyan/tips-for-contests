import Data.List
import Control.Monad
import Control.Applicative
import Control.Monad.State
-- import qualified Data.Vector as V
-- import Text.Regex.Posix
-- import Data.Bits

main = do
  n <- readLn :: IO Int
  putStrLn $ show $ fib n

mainFaked = do
  n <- readLn :: IO Int
  n:k:_ <- map read . words <$> getLine :: IO [Int]
  as <- map read . words <$> getLine :: IO [Integer]
  ss <- lines <$> getContents :: IO [String]
  vs <- map (map read . words) <$> lines <$> getContents :: IO [[Int]]
  vs <- replicateM n (map read . words <$> getLine) :: IO [[Int]]
  putStrLn $ show $ fib n
  putStr $ unlines $ sort ss
  putStr $ unlines $ map (unwords . map show) $ sortBy myCompare vs
  mapM (putStrLn) $ map (unwords . map show) $ sortBy myCompare vs

-- fold

foldlHist :: ([a] -> b -> [a]) -> [a] -> [b] -> a
foldlHist f a l = head $ foldl f a l

fib :: Int -> Integer
fib 0 = 0
fib 1 = 1
fib n = foldlHist step [fib 1, fib 0] [2..n]
    where step a@(a0:a1:_) n = (a0 + a1) : a

foldlState :: (c -> a -> b -> (c, a)) -> c -> a -> [b] -> a
foldlState f s a l = snd $ foldl (\(s,a) -> f s a) (s, a) l

foldlUntil :: (b -> Bool) -> (a -> b -> a) -> a -> [b] -> a
foldlUntil p f a [] = a
foldlUntil p f a (x:l)
    |       p x = a
    | otherwise = foldlUntil p f (f a x) l

foldS :: (a -> b -> c -> (a, c)) -> a -> [b] -> c -> a
foldS f a l s = evalS s $ foldM (\a -> \x -> (state $ f a x)) a l
evalS s m = evalState m s
execS s m = execState m s

-- find :: (a -> Bool) -> [a] -> Maybe a
-- find _ [] = Nothing
-- find p (x:l)
--     |       p x = Just x
--     | otherwise = find p l

findSurely :: (a -> Bool) -> [a] -> a
findSurely p l = head $ filter p l

-- sort

myCompare (ax:bx:_) (ay:by:_)
    |   bx < by = LT
    |   bx > by = GT
    | otherwise = EQ
myCompare _ _ = EQ

-- mod

addMod :: Int -> Int -> Int -> Int
addMod p x y = mod (x + y) p

mulMod :: Int -> Int -> Int -> Int
mulMod p x y = mod (x * y) p

invMod :: Int -> Int -> Int
invMod p x = mod (xgcd p x !! 2) p

xgcd :: Int -> Int -> [Int]
xgcd a b |     a < b = xgcdRec [0, 1, 1, 0] b a
         | otherwise = xgcdRec [1, 0, 0, 1] a b
         where xgcdRec m a 0 = a : m
               xgcdRec [x0,y0,x1,y1] a b = let d = div a b in xgcdRec [x1, y1, x0 - d * x1, y0 - d * y1] b (mod a b)

factMod :: Int -> Int -> Int
factMod p 0 = 1
factMod p n = mulMod p n $ factMod p (n-1)

cmbMod :: Int -> Int -> Int -> Int
cmbMod p n r = mulMod p (foldl (mulMod p) 1 [n-r+1..n]) (foldl (mulMod p) 1 $ map (invMod p) [1..r])

-- EOF

-- coding rules

-- ### no spacing
-- * pattern
-- * index
-- * easy arithmetic

-- ### additional spacing
-- * list comprehension
