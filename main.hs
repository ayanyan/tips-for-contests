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
foldlState f s a l = snd $ foldl (\(s, a) -> f s a) (s, a) l

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

-- EOF
