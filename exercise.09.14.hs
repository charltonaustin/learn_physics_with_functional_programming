type R = Double

a = const (-9.8)
initialV = 15.0
initialS = 0.0
stepSize = 1.0


calculateV stepSize v = v + a(stepSize/2.0) * stepSize
calculateS stepSize s v = s + calculateV(stepSize/2.0) v * stepSize


takeAStep  (t, s, v) = let nextT = t + stepSize
                           nextV = calculateV stepSize v
                           nextS = calculateS stepSize s v
                           in (nextT, nextS, nextV)
timePositionVelocity = iterate takeAStep (0, initialS, initialV)
