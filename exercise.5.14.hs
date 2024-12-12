expList :: Double -> [Double]
expList x = map (\n -> (1 + (x/n)) ** n) [1,2..]
expList' x = [ (1 + (x/n)) ** n | n <- [1, 2..]]

takeTillClose = takeWhile (\a -> (abs (a - 1)) < 0.01 ) (expList' 1)
