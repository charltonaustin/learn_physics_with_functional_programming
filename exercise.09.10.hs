type R = Double
tvPairs :: [(R, R)]
tvPairs = iterate tvUpdate (0, 0)

tvUpdate :: (R, R) -> (R, R)
tvUpdate (t, v) = (t + 1, v + 5 * t )
