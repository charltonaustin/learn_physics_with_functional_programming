expList :: Double -> [Double]
expList x = map (\n -> (1 + (x/n)) ** n) [1,2..]
expList' x = [ (1 + (x/n)) ** n | n <- [1, 2..]]
expDiff a = abs (a - (exp 10.0))
greaterThan1Percent a = a > 0.01
diffGreaterThanOnePercent = greaterThan1Percent . expDiff
takeTillClose x = takeWhile diffGreaterThanOnePercent (expList' x)
howBig = length (takeTillClose 10.0)

-- for 10 it is 107319505
-- for 1 it 134 
