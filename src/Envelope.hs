module Envelope where

  import qualified Ease

  type Sample = Float
  type Seconds = Float
  
  -- unit :: [Sample] -> [Sample]
  -- unit samples = samples

  attack :: Int -> [Sample] -> [Sample]
  attack bpmCount samples = ease 0 (take bpmCount samples) ++ drop bpmCount samples
    where
      easingFunction :: Float -> Float
      easingFunction = Ease.elasticOut (Ease.Amplitude 1) (Ease.Period 4)

      ease :: Int -> [Sample] -> [Sample]
      ease _ [] = []
      ease i (x:xs) = x * easingFunction (fromIntegral i / fromIntegral bpmCount) : ease (i + 1) xs

  decay :: Int -> [Sample] -> [Sample]
  decay bpmCount samples = reverse $ ease 0 (take bpmCount reversedSamples) ++ drop bpmCount reversedSamples
    where
      reversedSamples = reverse samples

      easingFunction :: Float -> Float
      easingFunction = Ease.sineIn

      ease :: Int -> [Sample] -> [Sample]
      ease _ [] = []
      ease i (x:xs) = x * easingFunction (fromIntegral i / fromIntegral bpmCount) : ease (i + 1) xs
      
