module Bank where

  import qualified Oscillator
  import qualified Envelope

  import Sound (sumNotes, sound, Note, note)

  bell :: Note
  bell = sumNotes (map sinSound freqs)
    where
      sinSound = sound Oscillator.sinWave
      freqs = [220, 440, 528, 660, 880, 1100, 1320, 1830]

  flat :: Note
  flat = sound Oscillator.sinWave 440