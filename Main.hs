module Main (main) where
import Control.Exception.Base
import System.Posix.Signals
import Control.Concurrent
import Control.Monad
import System.IO
import Network
import Network.IRC.Message

start :: PortID -> IO ()
start port = withSocketsDo $ do
  _ <- installHandler sigPIPE Ignore Nothing
  listen <- listenOn port
  forever $ accept listen >>= (forkIO . connection)

connection :: (Handle, HostName, PortNumber) -> IO ()
connection (h, _, _) = do
  hSetBuffering h NoBuffering
  loop `catch` ((\_ -> hClose h)::IOException -> IO ())
  where
   loop :: IO ()
   loop = do
    ready <- hWaitForInput h ((10::Int) ^ (6::Int))
    if not ready
      then hClose h
      else do
        line <- liftM2 (++) (hGetLine h) (return "\n")
        print line
        case parse message "" line of
          Left _ -> hPutStrLn h "use your words..."
          Right m -> hPutStrLn h "there's a good lad!" >> print m
        loop

main :: IO ()
main = start (PortNumber 12345)
