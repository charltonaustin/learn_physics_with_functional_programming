import Graphics.Gnuplot.Simple

type R = Double

type Time         = R
type TimeInterval = R
type Position     = R
type Velocity     = R

type Integration = (R -> R) -- function
                 -> R       -- lower limit
                 -> R       -- upper limit
                 -> R       -- result


integral :: R -> Integration
integral dt f a b = sum [ f t * dt | t <- [a + dt/2, a + 3 * dt/2 .. b - dt/2]]


-- A function that takes an initial velocity and returns a function
-- that takes in time and returns a height
yRock :: R -> R-> R
yRock initialV = integral 0.01 (vRock initialV) 0


-- A function that takes an initial velocity and returns a function
-- that takes in time and returns a velocity
vRock :: R -> (R -> R)
vRock initialV = \t -> initialV + (integral 0.1 (const (-9.8)) 0 t)


main :: IO ()
main = do
    let xValues :: [Double]
        xValues = [0.1, 0.2 .. 6]
    (plotFunc [] xValues (yRock 30.0))
