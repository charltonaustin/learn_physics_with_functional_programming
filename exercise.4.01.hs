type R = Double

type Time         = R
type TimeInterval = R
type Position     = R
type Velocity     = R


type PositionFunction = Time -> Position
type VelocityFunction = Time -> Velocity


type Derivative = (R -> R) -> R -> R

derivative :: R -> Derivative
derivative dt x t = (x (t + dt/2) - x (t - dt/2)) / dt

distanceFunction :: Time -> Position
distanceFunction t = t**2 / 2

vF10 :: Time -> Velocity
vF10 = derivative 10 distanceFunction

vF1 :: Time -> Velocity
vF1 = derivative 1 distanceFunction


-- This is nonexact because the binary representation of .1 is and infinite
-- repeating pattern so you must round which introduces a small error.
vF01 :: Time -> Velocity
vF01 = derivative 0.1 distanceFunction
