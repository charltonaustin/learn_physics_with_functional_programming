expSeries :: Double -> [Double]
expSeries x = repeat (exp x)

diff :: Double -> Double -> Double
diff a b = abs(a - b)

expDiff :: Double -> Double -> Double
expDiff x guess = diff (exp x) guess

greaterThan1Percent :: Double -> Bool
greaterThan1Percent x = (x >= 0.01)

greaterThan1PercentDiff :: Double -> Double -> Bool
greaterThan1PercentDiff x guess = greaterThan1Percent (expDiff x guess)

takeTillClose :: Double -> [Double]
takeTillClose x = takeWhile (greaterThan1PercentDiff x) (expSeries x)

howBig :: Double -> Int
howBig x = length (takeTillClose x)
