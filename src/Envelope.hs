module Envelope where

  import qualified Ease

  type Sample = Float
  type Seconds = Float
  
  bpm = 48000

  unit :: [Sample] -> [Sample]
  unit samples = samples

  attack :: Seconds -> [Sample] -> [Sample]
  attack duration samples = ease 0 (take bpmCount samples) ++ drop bpmCount samples
    where
      bpmCount :: Int
      bpmCount = floor (bpm * duration)

      easingFunction :: Float -> Float
      easingFunction = Ease.elasticOut (Ease.Amplitude 1) (Ease.Period 4)

      ease :: Int -> [Sample] -> [Sample]
      ease _ [] = []
      ease i (x:xs) = x * easingFunction (fromIntegral i / fromIntegral bpmCount) : ease (i + 1) xs

  decay :: Seconds -> [Sample] -> [Sample]
  decay duration samples = reverse $ ease 0 (take bpmCount reversedSamples) ++ drop bpmCount reversedSamples
    where
      reversedSamples = reverse samples

      bpmCount :: Int
      bpmCount = floor (bpm * duration)

      easingFunction :: Float -> Float
      easingFunction = Ease.sineIn

      ease :: Int -> [Sample] -> [Sample]
      ease _ [] = []
      ease i (x:xs) = x * easingFunction (fromIntegral i / fromIntegral bpmCount) : ease (i + 1) xs
      
