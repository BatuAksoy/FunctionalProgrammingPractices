f :: [Int] -> [Int]
f lst = filterIndex 0
    where
        filterIndex :: Int -> [Int]
        filterIndex i
            | i == length lst = []
            | otherwise = (if mod i 2 == 1 then [lst!!i] else []) ++ (filterIndex (i+1))

-- This part deals with the Input and Output and can be used as it is. Do not modify it.
main = do
	inputdata <- getContents
	mapM_ (putStrLn. show). f. map read. lines $ inputdata
