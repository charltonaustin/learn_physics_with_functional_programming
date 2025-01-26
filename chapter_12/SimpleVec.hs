{-# OPTIONS -Wall #-}

module SimpleVec where

infixl 6 ^+^
infixl 6 ^-^
infixr 7 *^
infixl 7 ^*
infixr 7 ^/
infixr 7 <.>
infixl 7 ><

type R = Double

data Mass = Mass R
            deriving (Eq, Show)

data Vec = Vec { xComp :: R
               , yComp :: R
               , zComp :: R
               } deriving (Eq)

instance Show Vec where
  show (Vec x y z) = "vec " ++ showDouble x ++ " "
                            ++ showDouble y ++ " "
                            ++ showDouble z
type Time = R
type PosVec = Vec
type Velocity = Vec
type Acceleration = Vec

type VecDerivative = (R -> Vec) -> R -> Vec

vecDerivative :: R -> VecDerivative
vecDerivative dt v t = (v (t + dt/2) ^-^ v (t - dt/2) ^/ dt)

velFromPos :: R                  -- dt
           -> (Time -> PosVec)   -- position function
           -> (Time -> Velocity) -- velocity function
velFromPos = vecDerivative

accFromVel :: R                      -- dt
           -> (Time -> Velocity)     -- velocity function
           -> (Time -> Acceleration) -- acceleration function
accFromVel = vecDerivative

positionCV :: PosVec -> Velocity -> Time -> PosVec
positionCV r0 v0 t = v0 ^* t ^+^ r0

velocityCA :: Velocity -> Acceleration -> Time -> Velocity
velocityCA v0 a0 t = a0 ^* t ^+^ v0

positionCA :: PosVec -> Velocity -> Acceleration -> Time -> PosVec
positionCA r0 v0 a0 t = 0.5 *^ t**2 *^ a0 ^+^ v0 ^* t ^+^ r0

aParallel :: Vec -> Vec -> Vec
aParallel v a = let vHat = v ^/ magnitude v
                in (vHat <.> a) *^ vHat

aPerp :: Vec -> Vec -> Vec
aPerp v a = a ^-^ aParallel v a

speedRateOfChange :: Vec -> Vec -> R
speedRateOfChange v a = (v <.> a) / magnitude v

radiusOfCurvature :: Vec -> Vec -> R
radiusOfCurvature v a = (v <.> v) / magnitude (aPerp v a)

projectilePos :: PosVec -> Velocity -> Time -> PosVec
projectilePos r0 v0 = positionCA r0 v0 (9.8 *^ negateV kHat)

showDouble :: R -> String
showDouble x
  | x < 0       = "(" ++ show x ++ ")"
  | otherwise   = show x

vec :: R
    -> R
    -> R
    -> Vec
vec = Vec

iHat :: Vec
iHat = Vec 1 0 0

jHat :: Vec
jHat = Vec 0 1 0

kHat :: Vec
kHat = Vec 0 0 1

zeroVec :: Vec
zeroVec = Vec 0 0 0

negateV :: Vec -> Vec
negateV (Vec ax ay az) = Vec (-ax) (-ay) (-az)

(^+^) :: Vec -> Vec -> Vec
Vec ax ay az ^+^ Vec bx by bz = Vec (ax + bx) (ay + by) (az + bz)

(^-^) :: Vec -> Vec -> Vec
Vec ax ay az ^-^ Vec bx by bz = Vec (ax - bx) (ay - by) (az - bz)

sumV :: [Vec] -> Vec
sumV = foldr (^+^) zeroVec

(*^) :: R -> Vec -> Vec
c *^ Vec ax ay az = Vec (c*ax) (c*ay) (c*az)

(^*) :: Vec -> R -> Vec
Vec ax ay az ^* c = Vec (c*ax) (c*ay) (c*az)

(<.>) :: Vec -> Vec -> R
Vec ax ay az <.> Vec bx by bz = (ax * bx) + (ay * by) + (az * bz)

(><) :: Vec -> Vec -> Vec
Vec ax ay az >< Vec bx by bz = Vec (ay * bz - az * by) (az * bx - ax * bz) (ax * by - ay * bx)

(^/) :: Vec -> R -> Vec
Vec ax ay az ^/ c = Vec (ax/c) (ay/c) (az/c)

magnitude :: Vec -> R
magnitude v = sqrt (v <.> v)
