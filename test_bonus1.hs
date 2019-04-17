module TestBonus1 where

import Bonus1



testDayOfWeek :: IO()
testDayOfWeek = do

  let inputs = [[2019, 4, 17], [1996, 1, 1]]
  let results = [4, 2]
  putStrLn $ if map (\t -> dayOfWeek (t!!0) (t!!1) (t!!2)) inputs == results then "dayOfWeek test passed" else "dayOfWeek test failed"

testLeap :: IO()
testLeap = do
  let inputs = [1996, 1997, 1998, 2016, 2019]
  let results = [True, False, False, True, False]
  putStrLn $ if map leap inputs == results then "leap test passed" else "leap test failed"

testDaysInMonth :: IO()
testDaysInMonth = do
  let inputs = [[4, 2019], [2, 1996], [7, 2018], [2, 1997]]
  let results = [30, 29, 31, 28]
  putStrLn $ if map (\t -> days_in_month (t!!0) (t!!1)) inputs == results then "days_in_month test passed" else "days_in_month test failed"


testSundays :: IO()
testSundays = do
  let inputs = [[1996, 1996], [1996, 1997], [2019, 2019], [1600, 1700]]
  let results = [2, 3, 2, 186]
  putStrLn $ if map (\t -> sundays1 (t!!0) (t!!1)) inputs == results then "sundays1 test passed" else "sundays1 test failed"
  putStrLn $ if map (\t -> sundays2 (t!!0) (t!!1)) inputs == results then "sundays2 test passed" else "sundays2 test failed"

main = do
  testDayOfWeek
  testLeap
  testDaysInMonth
  testSundays
