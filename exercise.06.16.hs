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
trapIntegrate n f a b =
  let delta = abs(a - b) / fromIntegral n
      smaller = if a < b then a else b
      larger = if b > a then b else a
  in (f(smaller) * delta/2)
     + sum [ delta * f(x)
           | x <- [smaller + delta
                  , smaller + (2 * delta)
                  .. smaller + ((fromIntegral (n-1)) * delta)]]
     + (f(larger) * delta/2)
