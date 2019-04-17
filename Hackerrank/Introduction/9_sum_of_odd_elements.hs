f :: [Int] -> Int
f arr = foldl (+) 0 (filter (\x -> (mod x 2) == 1) arr)

-- This part handles the Input/Output and can be used as it is. Do not change or modify it.
main = do
	inputdata <- getContents
	putStrLn $ show $ f $ map (read :: String -> Int) $ lines inputdata
