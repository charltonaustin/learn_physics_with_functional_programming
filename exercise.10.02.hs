import SimpleVec

takeAStep :: R -> (R -> Vec) -> Vec -> Vec
takeAStep dt f initial = initial ^+^ f (dt / 2) ^* dt

vecIntegral :: R           -- stepSize dt
            -> (R -> Vec)  -- vector-valued function
            -> R           -- lower limit
            -> R           -- upper limit
            -> Vec         -- result
vecIntegral dt f lower upper =
  let n = truncate ((upper - lower) / dt)
  in sumV (take n (iterate (takeAStep dt f) (f lower)))

-- this needs to be tested
