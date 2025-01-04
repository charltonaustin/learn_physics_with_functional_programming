type R = Double

initialV = 0.0

tvPairs :: [(R, R)]
tvPairs = iterate tvUpdate (0, initialV)

tvUpdate :: (R, R) -> (R, R)
tvUpdate (t, v) = (t + 1, initialV + 5 * (t + 1) )
