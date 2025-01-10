type R = Double

initialV = 0.0
a = const 5
stepSize = 1
tvPairs :: [(R, R)]
tvPairs = iterate tvUpdate (0, initialV)

tvUpdate :: (R, R) -> (R, R)
tvUpdate (t, v) = (t + stepSize, v + a(stepSize/2) * stepSize)
