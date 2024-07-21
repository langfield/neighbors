module Main (main, neighbors) where

import Data.List ((!?))
import Data.Maybe

neighbors :: a -> [[a]] -> [[[a]]]
neighbors d xss = map (map nbs) coords
  where
    (m, n) = (length xss, maybe 0 length (listToMaybe xss))
    directions = [(dx,dy) | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0]
    coords = [[(x, y) | y <- [0..n-1]] | x <- [0..m-1]]
    nb (x, y) (dx, dy) = fromMaybe d $ xss !? (x+dx) >>= (!? (y+dy))
    nbs e = map (nb e) directions

squarify :: [a] -> [[a]]
squarify xs = go . take (m * m) $ xs
  where
    m = floor $ sqrt (fromIntegral $ length $ xs :: Double)
    go [] = []
    go ys = take m ys : go (drop m ys)

reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

main :: IO ()
main = do
  putStrLn "hello world"
