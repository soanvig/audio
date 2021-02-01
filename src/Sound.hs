module Sound where
  import qualified Oscillator
  import qualified Envelope
  import qualified Math
  import Helpers

  type Frequency = Float
  type Seconds = Float
  type Volume = Float
  type Sample = Float
  type Note = [Sample]
  type Time = Float
  type ADSR = [Sample] -> [Sample]

  -- proportionally reduce signal amplitude to range <-1;1>
  normalizeVolume :: [Float] -> [Float]
  normalizeVolume list = map (* proportional) list
    where
      maxValue = Math.maxFrom (head list) list
      minValue = Math.minFrom (head list) list
      absMaxValue = max (abs maxValue) (abs minValue)
      proportional = case compare absMaxValue 1 of
        LT -> 1
        GT -> 1 / absMaxValue
        EQ -> 1
  
  sound :: (Time -> Sample) -> ADSR -> Frequency -> [Sample]
  sound waveFunction adsr freq = adsr $ map waveFunction $ Oscillator.generate freq duration
    where
      duration = 0.75

  note :: Float -> Note
  note n = sound Oscillator.sinWave Envelope.unit freq
    where
      baseNoteFreq :: Frequency
      baseNoteFreq = 440.0

      -- https://pages.mtu.edu/~suits/NoteFreqCalcs.html
      freq :: Frequency
      freq = baseNoteFreq * (2 ** (n / 12))
  
  delay :: Seconds -> Note -> Note
  delay secs note = zeros ++ note
    where
      zeros = replicate (floor (secs * Oscillator.bpm)) 0.0

  sumNotes :: [Note] -> Note
  sumNotes = foldl (zipWithPadded (+) 0 0) []

  