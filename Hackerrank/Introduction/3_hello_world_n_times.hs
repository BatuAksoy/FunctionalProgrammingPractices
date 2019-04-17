module Main where

main :: IO()
main = do
    n <- readLn :: IO Int
    putStrLn $ concat $ take n (repeat "Hello World\n")
