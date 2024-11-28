f :: Double -> Double
f x = sqrt (x + 1)

f' :: Double -> Double
f' x = sqrt $ x + 1

f'' :: Double -> Double
f'' = sqrt . (+ 1)
