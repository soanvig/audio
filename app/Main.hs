module Main where

  -- external
  import qualified Data.ByteString.Lazy as LazyBS
  import qualified Data.ByteString as BS
  import qualified Data.ByteString.Builder as BS
  import Data.Foldable (fold)
  import System.Process (runCommand)

  -- internal
  import Sound (gain)
  import Bank (bell, flat)
  import Envelope (attack, decay)
  import Keyboard (getKey, initKeyboard)
  import AudioSystem


  toBS :: [Float] -> BS.ByteString
  toBS = LazyBS.toStrict . BS.toLazyByteString . foldMap BS.floatLE

  main = do
    initKeyboard
    initAudio

    let pcm = toBS $ take 44000 $ gain 0.03 bell
    sample <- getSample pcm

    playOnKey sample

    finishAudio

  playOnKey sample = do
    key <- getKey
    _ <- playSample sample
    playOnKey sample
    