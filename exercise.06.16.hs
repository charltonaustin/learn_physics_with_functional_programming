type R = Double

type Time         = R
type TimeInterval = R
type Position     = R
type Velocity     = R

trapIntegrate :: Int       -- # of trapezoids n
              -> (R -> R)  -- function f
              -> R         -- lower limit a
              -> R         -- upper limit b
              -> R         -- result
trapIntegrate n f a b = undefined
