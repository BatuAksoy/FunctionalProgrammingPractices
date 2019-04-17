import Data.List

intToStr :: Int->String
intToStr num = show num

toCommaSeparatedString :: [Int] -> String
toCommaSeparatedString lst = (intercalate ", " (map intToStr lst))

fn2 :: Int -> [Int]
fn2 n = [0..(n-1)]

fn :: Int -> String
fn n = toCommaSeparatedString . fn2 $ n

main = do
  n <- readLn :: IO Int
  print (fn(n))
