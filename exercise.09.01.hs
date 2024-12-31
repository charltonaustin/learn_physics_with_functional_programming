type R = Double
polarToCart :: (R, R) -> (R, R)
polarToCart (r, theta) = (r * cos theta, r * sin theta)
