module Main where

  -- external
  import qualified Data.ByteString.Lazy as BS
  import qualified Data.ByteString.Builder as BS
  import Data.Foldable (fold)
  import System.Process (runCommand)

  -- internal
  import Sound (note, sumNotes, sound, volumeNormalization, delay)
  import qualified Wave

  save :: [Float] -> IO ()
  save floats = BS.writeFile "output.bin" $ BS.toLazyByteString $ foldMap BS.floatLE floats

  sinSound = sound Wave.sinWave

  main :: IO ()
  main = do
    let bell = sumNotes [sinSound 220, sinSound 440, sinSound 528, sinSound 660, sinSound 880, sinSound 1100, sinSound 1320, sinSound 1830]
    save $ volumeNormalization $ sumNotes [bell, delay 1.5 bell, delay 3 bell]
    _ <- runCommand "ffplay -showmode 1 -f f32le -ar 48000 output.bin"
    return ()
