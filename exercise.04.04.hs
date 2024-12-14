type R = Double

type Time         = R
type TimeInterval = R
type Position     = R
type Velocity     = R
type Acceleration = R

type PositionFunction = Time -> Position
type VelocityFunction = Time -> Velocity


type Derivative = (R -> R) -> R -> R

derivative :: R -> Derivative
derivative dt x t = (x (t + dt/2) - x (t - dt/2)) / dt


pos1 :: Time -> Position
pos1 t =
  if t < 0
    then 0
    else 5 * t ** 2

vel1Analytic :: Time -> Velocity
vel1Analytic t =
  if t < 0
     then 0
     else 10 * t

acc1Analytic :: Time -> Acceleration
acc1Analytic t =
  if t < 0
    then 0
    else 10.0

vel1Numerical :: Time -> Velocity
vel1Numerical t = derivative 0.01 pos1 t

acc1Numerical :: Time -> Acceleration
acc1Numerical t = derivative 0.01 vel1Numerical t

greaterThan1Percent :: R -> R -> Bool
greaterThan1Percent actualValue testValue = (testValue / actualValue) > 0.01

distance :: R -> R -> R
distance a b = abs (a - b)

ts = [0.1, 0.2 .. 100]

testValues = map vel1Numerical ts
actualValues = map vel1Analytic ts
distances = zipWith distance testValues actualValues

tsAndDistances = zip actualValues distances

largeDistances = filter (\x -> greaterThan1Percent (fst x) (snd x)) tsAndDistances 



