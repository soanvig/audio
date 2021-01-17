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

  sound :: (Time -> Sample) -> Frequency  -> [Sample]
  sound waveFunction freq = adsr $ map ((* volume) . waveFunction) $ Wave.generate freq duration
    where
      volume = 0.2
      duration = 3
      adsr = Envelope.decay 2.5 . Envelope.attack 0.5

  note :: Float -> Note
  note n = sound Wave.sinWave freq
    where
      baseNoteFreq :: Frequency
      baseNoteFreq = 440.0

      -- https://pages.mtu.edu/~suits/NoteFreqCalcs.html
      freq :: Frequency
      freq = baseNoteFreq * (2 ** (n / 12))
  
  delay :: Seconds -> Note -> Note
  delay secs note = zeros ++ note
    where
      zeros = replicate (floor (secs * Wave.bpm)) 0.0

  sumNotes :: [Note] -> Note
  sumNotes = foldl (zipWithPadded (+) 0 0) []