module Bank where

  import qualified Oscillator
  import qualified Envelope

  import Sound (sumNotes, sound, Note)

  bell :: Note
  bell = map (0.1 *) $ sumNotes (map sinSound freqs)
    where
      sinSound = sound Oscillator.sinWave (Envelope.attack 0.5 . Envelope.decay 1)
      freqs = [220, 440, 528, 660, 880, 1100, 1320, 1830]