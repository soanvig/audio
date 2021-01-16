module Math where

  maxFrom :: Float -> [Float] -> Float
  maxFrom x [] = x
  maxFrom x (y:ys) = max x (maxFrom y ys)

  minFrom :: Float -> [Float] -> Float
  minFrom x [] = x
  minFrom x (y:ys) = min x (minFrom y ys)
