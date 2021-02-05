module AudioSystem where

  import Sound.ProteaAudio as PA
  import Control.Monad
  import Control.Concurrent
  import qualified Data.ByteString as BS

  import qualified Config
    
  initAudio = do
    audioSystem <- PA.initAudio 64 Config.freq 1024
    unless audioSystem $ fail "failed to initialize the audio system"

  finishAudio = PA.finishAudio

  waitPlayback = do
    n <- PA.soundActiveAll
    when  (n > 0) $ do
      threadDelay (1000 * 100)
      waitPlayback

  getSample :: BS.ByteString -> IO PA.Sample
  getSample pcm = sampleFromMemoryPcm pcm 1 Config.halfFreq 32 0.5

  playSample :: PA.Sample -> IO PA.Sound
  playSample sample = PA.soundPlay sample 1 1 0 ((2 ** (5 / 12)) - 0.5)
    


