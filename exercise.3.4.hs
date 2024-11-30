bag2Fee :: Bool -> Int
bag2Fee hasABag =
  if hasABag
    then 100
    else 0

bag2Fee' :: Bool -> Int
bag2Fee' hasABag =
  case hasABag of
    True -> 100
    False -> 0
