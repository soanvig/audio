module Oscillator where

  type Frequency = Float
  type Seconds = Float
  type Time = Float
  type Sample = Float
  type Duration = Seconds

  bpm = 48000

  generate :: Frequency -> Duration -> [Time]
  generate freq duration = map (* step) [0.0 .. bpm * duration]
    where
      step = freq * 2 * pi / bpm

  sinWave :: Time -> Sample
  sinWave = sin

  squareWave :: Time -> Sample
  squareWave = signum . sinWave

  triangleWave :: Time -> Sample
  triangleWave v = (2 * a / pi) * asin ( sin ( 2 * pi / p * v))
    where
      a = 1
      p = 6.15 -- tuned, to match 220hz example in https://en.wikipedia.org/wiki/Triangle_wave

  -- sawtooth over sin ? not sure
  -- interesting effect though
  sawtoothWave :: Time -> Sample
  sawtoothWave v = sinV - abs sinV
    where
      sinV = sin v

  sawtoothWave2 :: Time -> Sample
  sawtoothWave2 v = ((-2) * a / pi) * atan (1 / tan ( v * pi / p))
    where
      a = 1
      p = 6.3 -- tuned, to match 220hz example in https://en.wikipedia.org/wiki/Sawtooth_wave