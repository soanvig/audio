module Keyboard where
  
  import System.IO (hSetBuffering, hSetEcho, stdin, BufferMode(NoBuffering), getChar, hReady, Handle)

  initKeyboard :: IO ()
  initKeyboard = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False

  getKey :: IO String
  getKey = reverse <$> getKey' ""
    where
      getKey' chars = do
        char <- getChar
        more <- hReady stdin
        (if more then getKey' else return) (char:chars)