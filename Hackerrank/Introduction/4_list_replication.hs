f :: Int -> [Int] -> [Int]
f n arr = concat (map ((take n) . repeat) arr)

-- Main is given
main :: IO ()
main = getContents >>=
       mapM_ print. (\(n:arr) -> f n arr). map read. words
