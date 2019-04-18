module Bonus1 where

-- 1. dayOfWeek
-- Saturday = 0, Sunday = 1
dayOfWeek :: Int -> Int -> Int -> Int
dayOfWeek y m d = mod result 7
    where
      m' = if m < 3 then m+12 else m
      y' = if m < 3 then y-1 else y
      k  = mod y' 100
      j  = div y' 100
      result = d + (div (13*(m'+1)) 5) + k + (div k 4) + div j 4 + 5*j

-- 2. Fill sundays1
sundays1 :: Int -> Int -> Int
sundays1 start end = sundays' start 1
  where
      sundays' :: Int -> Int -> Int
      sundays' y m
        | y > end = 0
        | otherwise = if dayOfWeek y m 1 == 1 then rest + 1 else rest
        where
            nextY = if m == 12 then y+1 else y
            nextM = if m == 12 then 1 else m+1
            rest = sundays' nextY nextM

-- 3. Make sundays1 tail recursive
sundays1Tail :: Int -> Int -> Int
sundays1Tail start end = sundays' 0 start 1
  where
      sundays' :: Int -> Int -> Int -> Int
      sundays' acc y m
        | y > end = acc
        | otherwise = if dayOfWeek y m 1 == 1 then rest 1 else rest 0
        where
            nextY = if m == 12 then y+1 else y
            nextM = if m == 12 then 1 else m+1
            rest num = sundays' (acc+num) nextY nextM
-- 4. util:leap
leap :: Int -> Bool
leap y = (mod y 4 == 0) && (mod y 100 /= 0) || (mod y 400 == 0)

-- 4. util:days_in_month
days_in_month :: Int -> Int -> Int
days_in_month m y
    | m == 2 = if leap y then 29 else 28
    | elem m [4,6,9,11] = 30
    | otherwise = 31

-- 4. implement sundays2
sundays2 :: Int -> Int -> Int
sundays2 start end = sundays' firstDay 0 start 1
    where
      firstDay' = dayOfWeek start 1 1
      firstDay = if firstDay' > 0 then firstDay' else firstDay' + 7
      sundays' :: Int -> Int -> Int -> Int -> Int
      sundays' weekday n y m
          | y > end = n
          | otherwise = sundays' nextWeekDays nextN nextY nextM
              where
                nextWeekDays = weekday + (days_in_month m y)
                nextY = if m == 12 then y+1 else y
                nextM = if m == 12 then 1 else m+1
                nextN = if mod nextWeekDays 7 == 0 then n+1 else n
