{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import Data.List (take, drop)
import Data.Text.Read
import Data.Text (Text)
import qualified Data.Text as T (lines, splitOn)
import qualified Data.Text.IO as TIO (getLine, getContents)

main :: IO ()
main = do
      lines <- readLines
      let input = map (map num) $ map (T.splitOn " ") lines
      putStrLn $ show $ solve input

num :: Text -> Int
num = either (error . show) fst
    . signed decimal

readLines :: IO [Text]
readLines = T.lines <$> TIO.getContents

input :: IO [[Int]]
input = return [ [0, 3, 6, 9, 12, 15]
        , [1, 3, 6, 10, 15, 21]
        , [10, 13, 16, 21, 30, 45]
        ]

solve :: [[Int]] -> (Int, Int)
solve input = foldl (\(a1, a2) (b1, b2) -> (a1 + b1, a2 + b2)) (0, 0) (map extrapolate input)

extrapolate :: [Int] -> (Int, Int)
extrapolate series = up (down series) 0 0

down :: [Int] -> [[Int]]
down series = down' series [series]
down' series acc =
    let
      n = length series
      nextStage = zipWith (-) (drop 1 series) $ take (n - 1) series
    in
      case all (== 0) nextStage of
          True -> acc
          False -> down' nextStage (nextStage:acc)

up :: [[Int]] -> Int -> Int -> (Int, Int)
up [] front back = (front, back)
up (stage@(fst:_):stages) front back =
    let lst = last stage
    in up stages (front + lst) (fst - back)
