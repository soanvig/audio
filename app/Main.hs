module Main where

  -- external
  import qualified Data.ByteString.Lazy as BS
  import qualified Data.ByteString.Builder as BS
  import Data.Foldable (fold)
  import System.Process (runCommand)

  -- internal
  import Sound (note, sumNotes, sound, volumeNormalization)

  save :: [Float] -> IO ()
  save floats = BS.writeFile "output.bin" $ BS.toLazyByteString $ foldMap BS.floatLE floats

  main :: IO ()
  main = do
    save $ sound 220
    _ <- runCommand "ffplay -showmode 1 -f f32le -ar 48000 output.bin"
    return ()
