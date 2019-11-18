import Data.List
import qualified Data.Vector as V

main = do
  n <- readLn :: IO Int
  putStrLn $ show $ factVec n V.! n
  -- n:r:_ <- map read . words <$> getLine :: IO [Int]
  -- putStrLn $ show $ cmb n r
  -- n:w:_ <- map read . words <$> getLine :: IO [Int]
  -- xs <- map (map read . words) <$> lines <$> getContents :: IO [[Int]]
  -- putStrLn $ show $ knapsack xs w


factVec :: Int -> V.Vector Integer
factVec 0 = V.singleton 1
factVec n = let v = factVec (n-1) in V.snoc v $ (v V.! (n-1)) * fromIntegral n

cmb :: Int -> Int -> Integer
cmb n r = cmbMemo n V.! r
    where cmbMemo :: Int -> V.Vector Integer
          cmbMemo 1 = V.fromList [1, 1]
          cmbMemo n =
              let memo = cmbMemo (n-1) in
              V.fromList [ (if r == 0 then 0 else memo V.! (r-1)) + (if r == n then 0 else memo V.! r) | r <- [0..n] ]

knapsack :: [[Int]] -> Int -> Int
knapsack xs k = knapsackMemo xs V.! k
    where knapsackMemo :: [[Int]] -> V.Vector Int
          knapsackMemo [] = V.replicate (k+1) 0
          knapsackMemo ([v,w]:xs) =
              let memo = knapsackMemo xs in
              V.generate (k+1) $ \j -> max (memo V.! j) (if j < w then 0 else (memo V.! (j-w)) + v)
