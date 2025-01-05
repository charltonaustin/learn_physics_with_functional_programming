type R = Double

a = -9.8
initialV = 15.0
initialS = 0.0
stepSize = 1.0

createUpdateV :: R -> R
createUpdateV = \t -> (initialV + a * t)

createUpdateS = \t -> initialS + initialV * t + 0.5 * a * t ** 2

updateV :: R -> R
updateV t = createUpdateV t

updateS :: R -> R
updateS t = createUpdateS t

takeAStep  (t, s, v) = let nextT = t + stepSize
                           nextV = updateV nextT
                           in (nextT, updateS nextT, nextV)
timePositionVelocity = iterate takeAStep (0, initialS, initialV)
