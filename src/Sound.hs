module Sound where
  import qualified Wave
  import qualified Envelope
  import qualified Math
  import Helpers

  type Frequency = Float
  type Seconds = Float
  type Volume = Float
  type Sample = Float
  type Note = [Sample]
  type Time = Float

  -- proportionally reduce signal amplitude to range <-1;1>
  volumeNormalization :: [Float] -> [Float]
  volumeNormalization list = map (* proportional) list
    where
      maxValue = Math.maxFrom (head list) list
      minValue = Math.minFrom (head list) list
      absMaxValue = max (abs maxValue) (abs minValue)
      proportional = case compare absMaxValue 1 of
        LT -> 1
        GT -> 1 / absMaxValue
        EQ -> 1

  sound :: Frequency -> (Time -> Sample) -> [Sample]
  sound freq waveFunction = adsr $ map ((* volume) . waveFunction) $ Wave.generate freq duration
    where
      volume = 0.2
      duration = 5
      adsr = Envelope.decay 0.2 . Envelope.attack 0.3

  note :: Float -> Note
  note n = sound freq Wave.sinWave
    where
      baseNoteFreq :: Frequency
      baseNoteFreq = 440.0

      -- https://pages.mtu.edu/~suits/NoteFreqCalcs.html
      freq :: Frequency
      freq = baseNoteFreq * (2 ** (n / 12))

  sumNotes :: [Note] -> Note
  sumNotes = foldl (zipWithPadded (+) 0 0) []