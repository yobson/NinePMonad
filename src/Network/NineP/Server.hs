{-# LANGUAGE RankNTypes, OverloadedStrings, ViewPatterns, RecordWildCards, TypeApplications, FlexibleContexts, MonoLocalBinds #-}

{-|
Module      : Network.NineP.Server
Description : Running file system servers
Maintainer  : james@hobson.space
Copyright   : (c) James Hobson, 2025
Stability   : experimental
Portability : POSIX
-}

module Network.NineP.Server
( serveFileSystem
, hoistFileSystemServer
, defaultConf
, BindAddr(..)
, FSServerConf(..)
, LogLevel(..)
)
where

import Data.Word

import Control.Monad.Fix
import Control.Applicative
import Data.Attoparsec.Text
import Data.Text (pack, unpack)
import Data.String
import Network.Run.TCP
import Network.Socket
import Control.Monad
import Control.Monad.IO.Class
import qualified Control.Exception as E
import Control.Concurrent (forkFinally)
import System.Posix.Env
import System.FilePath
import System.Posix.Temp
import System.Posix.Process

import Network.NineP.Monad
import Network.NineP.Effects
import Network.NineP.Handler (handleRequest)

-- | Server configuration
data FSServerConf = FSServerConf
  { bindAddr  :: BindAddr    -- ^ Has a 'IsString' instance. You can provide a string such as @"tcp!192.168.2.4!8080"@
  , logLevels :: [LogLevel]  -- ^ Specify what kind of message to log
  }

defaultConf :: BindAddr -> FSServerConf
defaultConf addr = FSServerConf
  { bindAddr = addr
  , logLevels = []
  }

data BindAddr = UnixDomain FilePath | Tcp HostName Word16 | Path FilePath
  deriving Eq

ninePBindFmt :: Parser BindAddr
ninePBindFmt =  UnixDomain <$> (asciiCI "unix!" *> filePathFmt)
            <|> Tcp <$> (asciiCI "tcp!" *> ipFmt) <*> ("!" *> decimal)
            <|> Tcp <$> (asciiCI "tcp!" *> ipFmt) <*> pure 564
            <|> Path <$> filePathFmt

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
  show (Path path)     = path


-- | Host file server defined in terms of 'FileSystem'
serveFileSystem :: MonadIO m => FSServerConf -> FileSystem () -> m ()
serveFileSystem conf = hoistFileSystemServer conf strictID

strictID :: a -> a
strictID !x = x

-- | Host file server defined on arbitary monad by providing a natural transformation
hoistFileSystemServer :: (MonadIO n, MonadIO m, MonadFail n) => FSServerConf -> (forall x . n x -> IO x) -> FileSystemT n () -> m ()
hoistFileSystemServer FSServerConf{..} hoist fs = do
  globalState <- liftIO $ mkGlobalState fs
  runServer bindAddr $ \sock -> do
      err <- runM $ runError @NPError
                  $ runFilterLogger logLevels
                  $ runMsgHandle sock
                  $ runGlobalState globalState hoist
                  $ runClientState
                  $ fix $ \loop -> do
                    msg <- recvMsg
                    handleRequest msg
                    loop
      case err of
        Left e  -> error $ "Something went wrong: " <> e
        Right a -> return a


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
runServer (Tcp host port) action = liftIO $ runTCPServer (Just host) (show port) action
runServer (UnixDomain fp) action = liftIO $ runUnixServer fp action
runServer (Path path) action = liftIO $ do
  p9dir' <- getEnv "PLAN9"
  case p9dir' of
    Nothing -> error "set PLAN9 environment variable"
    Just p9dir -> do
      let bin = p9dir </> "bin" </> "9pfuse"
      tmpdir <- mkdtemp ".9pmonad"
      let socketFile = tmpdir </> "filesystem.sock"
      _ <- forkProcess $ void $ runServer (UnixDomain socketFile) action
      executeFile bin False [socketFile, path] Nothing
