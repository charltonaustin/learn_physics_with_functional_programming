import SimpleVec
import Data.List (maximumBy)

type Velocity = Vec
type PosVec = Vec

a :: R -> Vec
a x = Vec 0 0 (-9.8)
stepSize = 0.01


takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil predicate (x:xs)
  | predicate x = [x]
  | otherwise   = x : takeUntil predicate xs

calculateV :: R -> Vec -> Vec
calculateV stepSize v = v ^+^ a(stepSize/2.0) ^* stepSize

calculateS :: R -> Vec -> Vec -> Vec
calculateS stepSize s v = s ^+^ calculateV(stepSize/2.0) v ^* stepSize

takeAStep :: (R, Vec, Vec) -> (R, Vec, Vec)
takeAStep (t, s, v) =
  let nextT = t + stepSize
      nextV = calculateV stepSize v
      nextS = calculateS stepSize s v
      in (nextT, nextS, nextV)

timePositionVelocity :: Vec -> Vec -> [(R, Vec, Vec)]
timePositionVelocity initialS initialV = iterate takeAStep (0, initialS, initialV)

second :: (a, b, c) -> b
second (_, y, _) = y

lessThanZero :: (R, Vec, Vec) -> Bool
lessThanZero vTP = (zComp (second vTP)) < 0

validTimePosVel :: Vec -> Vec -> [(R, Vec, Vec)]
validTimePosVel initialPos initialVelocity =
  takeUntil  lessThanZero (timePositionVelocity initialPos initialVelocity)


toPos :: (R, Vec, Vec) -> R
toPos = undefined

allPos :: Vec -> Vec -> [R]
allPos initialPos initialVel = map toPos (validTimePosVel initialPos initialVel)

maxHeight :: PosVec -> Velocity -> R
maxHeight initialPos initialVelocity =
  maximum (allPos initialPos initialVelocity)

-- this needs to be tested
