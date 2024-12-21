type R = Double

type Time         = R
type TimeInterval = R
type Position     = R
type Velocity     = R

type Integration = (R -> R) -> R

integral :: R -> Integration
integral dt f a b = sum [ f t * dt | t <- [a + dt/2, a + 3 * dt/2.. b - dt/2]]
-- A function that takes an initial velocity and returns a function
-- that takes in time and returns a height
yRock :: R -> R-> R
yRock = undefined


-- A function that takes an initial velocity and returns a function
-- that takes in time and returns a velocity
vRock :: R -> R -> R
vRock = undefined
