-- First Haskell Program (for learning physics)

-- Here we define a constant
e :: Double
e = exp 1

-- Here we define a function
square :: Double -> Double
square x = x**2

yRock30 :: Double -> Double
yRock30 t = (30 * t) - ((1/2) * 9.8 * t ** 2)

vRock30 :: Double -> Double
vRock30 t = (-9.8) * t + 30

sinDeg :: Double -> Double
sinDeg = sin . (* (pi / 180))
