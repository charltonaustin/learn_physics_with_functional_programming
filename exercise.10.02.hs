import SimpleVec

vecIntegral :: R           -- stepSize dt
            -> (R -> Vec)  -- vector-valued function
            -> R           -- lower limit
            -> R           -- upper limit
            -> Vec         -- result
vecIntegral dt f a b =
  sumV [ f t ^* dt | t <- [a + dt/2.0, a + 3 * dt/2.0 .. b - dt/2.0]]
fnVec x = Vec (x**3) (exp(1)**(-x ** 2)) 1

-- test vecIntegral 0.0001 fnVec 0 1
