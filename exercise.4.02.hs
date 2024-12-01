import Data.List (zipWith)

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

f :: R -> R
f x = x ** 3

dfa :: R -> R
dfa x = 3 * x  ** 2

dfn1 :: R -> R
dfn1 = derivative 1 f

stepList :: [Double]
stepList = [-10, -9.75 .. 10]

exploreValues :: (Double -> Double) -> [Double]
exploreValues df = map df stepList

absDifference :: Num a => [a] -> [a] -> [a]
absDifference xs ys = zipWith (\x y -> abs (x - y)) xs ys

-- absDifference (exploreValues dfa) (exploreValues dfn1) always 0.25

dfna :: R -> (R -> R)
dfna a = derivative a f

roundTo :: Int -> Double -> Double
roundTo n d = (fromInteger $ round $ d * (10^n)) / (10.0^^n)

as :: [Double]
as = map (roundTo 1) [0.1, 0.2 .. 3.0]

das :: [R -> R]
das = map dfna as

exploreValuesS = map exploreValues das

differenceFinder = absDifference (exploreValues dfa)

differences = map differenceFinder exploreValuesS

-- a = 1.385655 gives an error of 1.0000207182421819 percent at x = 4
-- a = 0.0346411 gives an error of 1.0000054183600946 percent at x = 0.1
