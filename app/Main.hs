module Main where

  -- external
  import qualified Data.ByteString.Lazy as LazyBS
  import qualified Data.ByteString as BS
  import qualified Data.ByteString.Builder as BS
  import Data.Foldable (fold)
  import System.Process (runCommand)
  import Sound.ProteaAudio
  import Control.Monad
  import Control.Concurrent

  -- internal
  import Sound (gain)
  import Bank (bell, flat)
  import Envelope (attack, decay)

  import System.IO (hSetBuffering, hSetEcho, stdin, BufferMode(NoBuffering), getChar, hReady)

  toBS :: [Float] -> BS.ByteString
  toBS = LazyBS.toStrict . BS.toLazyByteString . foldMap BS.floatLE

  main = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False

    result <- initAudio 64 44100 1024 -- max channels, mixing frequency, mixing buffer size
    unless result $ fail "failed to initialize the audio system"

    let pcm = toBS $ (attack 500 . decay 500) $ take 44100 $ gain 0.03 bell
    sample <- sampleFromMemoryPcm pcm 1 22050 32 0.5

    playOnKey sample

    finishAudio

  waitPlayback = do
    n <- soundActiveAll
    when  (n > 0) $ do
      threadDelay (1000 * 100)
      waitPlayback

  playOnKey sample = do
    key <- getKey
    let num = read key
    _ <- soundPlay sample 1 1 0 ((2 ** (num / 12)) - 0.5)
    playOnKey sample
    

  getKey :: IO [Char]
  getKey = reverse <$> getKey' ""
    where
      getKey' chars = do
        char <- getChar
        more <- hReady stdin
        (if more then getKey' else return) (char:chars)