import SimpleVec

type Velocity = Vec
type PosVec = Vec

type Acceleration = R -> Vec

type Time = R
type Speed = R

stepSize :: Time
stepSize = 0.01


takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil predicate (x:xs)
  | predicate x = [x]
  | otherwise   = x : takeUntil predicate xs

first :: (a, b, c) -> a
first (t, _, _) = t

second :: (a, b, c) -> b
second (_, v, _) = v

calculateV :: Velocity -> Acceleration -> R -> Velocity
calculateV v a s = v ^+^ a(s/2.0) ^* s

takeAStep :: (Time, Velocity, Acceleration) -> (Time, Velocity, Acceleration)
takeAStep (t, v, a) =(t + stepSize, (calculateV v a stepSize), a)

timeIsClose :: Time -> (Time, Velocity, Acceleration) -> Bool
timeIsClose t tVA = (first tVA) >= t

validTimeVelAcc :: Velocity -> Acceleration -> [(Time, Velocity, Acceleration)]
validTimeVelAcc initialV acc = iterate takeAStep (0, initialV, acc)

finalVelocity :: Time -> Velocity -> Acceleration -> Velocity
finalVelocity t initialV acc =
  second (last (takeUntil (timeIsClose t) (validTimeVelAcc initialV acc)))

speedCA :: Velocity -> Acceleration -> Time -> Speed
speedCA initialV acc = \t -> magnitude (finalVelocity t initialV acc)
