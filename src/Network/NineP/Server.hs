{-# LANGUAGE RankNTypes, OverloadedStrings, ViewPatterns, RecordWildCards, TypeApplications #-}

module Network.NineP.Server where

import Effectful
import Data.Word

import Control.Monad.Fix
import Control.Applicative
import Data.Attoparsec.Text
import Data.Text (pack, unpack)
import Data.String
import Network.Run.TCP
import Network.Socket
import Control.Monad
import qualified Control.Exception as E
import Control.Concurrent (forkFinally)

import Network.NineP.Monad
import Network.NineP.Handler
import Network.NineP.Effects

data FSServerConf = FSServerConf
  { bindAddr  :: BindAddr
  , debugLogs :: Bool
  }

data BindAddr = UnixDomain FilePath | Tcp HostName Word16
  deriving Eq

ninePBindFmt :: Parser BindAddr
ninePBindFmt =  UnixDomain <$> (asciiCI "unix!" *> filePathFmt)
            <|> Tcp <$> (asciiCI "tcp!" *> ipFmt) <*> ("!" *> decimal)
            <|> Tcp <$> (asciiCI "tcp!" *> ipFmt) <*> pure 564

instance IsString BindAddr where
  fromString (pack -> t) = case parseOnly (ninePBindFmt <* endOfInput) t of
                             Left  s   -> error s
                             Right res -> res

filePathFmt :: Parser FilePath
filePathFmt = manyTill anyChar endOfInput

ipFmt :: Parser String
ipFmt = unpack <$> takeTill (== '!')

instance Show BindAddr where
  show (UnixDomain fp) = "unix!" <> fp
  show (Tcp ip 564)    = "tcp!"  <> ip
  show (Tcp ip port)   = "tcp!"  <> ip <> "!" <> show port


serveFileSystem :: MonadIO m => FSServerConf -> FileSystem () -> m ()
serveFileSystem conf = hoistFileSystemServer conf id

hoistFileSystemServer :: (MonadIO n, MonadIO m) => FSServerConf -> (forall x . n x -> IO x) -> FileSystemT n () -> m ()
hoistFileSystemServer FSServerConf{..} hoist fs = runServer bindAddr $ \sock -> runAppThrow debugLogs sock fs hoist $ fix $ \loop -> do
  msg <- recvMsg
  res <- tryError @NPError $ handleRequest msg
  case res of
    Left (_,e) -> logMsg Fatal $ "Thread closed due to error: " <> show e
    Right _    -> loop

runUnixServer :: FilePath -> (Socket -> IO a) -> IO a
runUnixServer fp server = E.bracket openSock close loop
    where open = E.bracketOnError (openSocket addr) close
          openSock = open $ \sock -> do
            bind sock addrAddress
            listen sock 5
            return sock
          loop sock = forever $ E.bracketOnError (accept sock) (close . fst) $ \(conn, _peer) -> do
            forkFinally (server conn) (const $ gracefulClose conn 5000)
          addr           = AddrInfo{..}
          addrFlags      = [AI_PASSIVE]
          addrFamily     = AF_UNIX
          addrSocketType = Stream
          addrProtocol   = defaultProtocol
          addrAddress    = SockAddrUnix fp
          addrCanonName  = Nothing

runServer :: (MonadIO m) => BindAddr -> (Socket -> IO a) -> m a
runServer (Tcp host port) = liftIO . runTCPServer (Just host) (show port)
runServer (UnixDomain fp) = liftIO . runUnixServer fp
