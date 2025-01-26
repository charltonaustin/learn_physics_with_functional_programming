amazingCurve :: Int -> Int
amazingCurve score =
  let newScore = score * 2
   in if newScore > 100
        then 100
        else newScore
