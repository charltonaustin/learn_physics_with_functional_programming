factorial n = foldr (*) 1 (take n [1,2..])
