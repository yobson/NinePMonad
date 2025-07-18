{-# LANGUAGE GeneralizedNewtypeDeriving, FunctionalDependencies, DataKinds, GADTs,
             FlexibleContexts, TypeFamilies, AllowAmbiguousTypes, UndecidableInstances,
             FlexibleInstances, ScopedTypeVariables, PolyKinds, RankNTypes, OverloadedLabels, 
             RecordWildCards, ViewPatterns, TemplateHaskell, StrictData
#-}

{-|
Module      : Network.NineP.Monad
Description : Monad for constructing file systems
Maintainer  : james@hobson.space
Copyright   : (c) James Hobson, 2025
Stability   : experimental
Portability : POSIX

You use this module to build file system trees. These trees are represented as monads
and most start with a root directory:

> dir "/" $ do
>   file "file1"
>   file "file2"

You can add attributes to directories and files:

> dir "/" [#owner := "bob"] $ ...

The full list of attributes are:

* @perms@
* @owner@
* @group@

Files also recive 'Reader' and 'Writer' objects:

> dir "/" $ do
>   file "file1" (Reader $ return "Hello world")
>   file "file2" (Writer $ B.putStrLn)
-}

module Network.NineP.Monad
( FileSystemT(..)
, FileSystemFT(..)
, FileSystem
, Reader(..)
, Writer(..)
, file
, dir
, Attribute(..)
, File(..)
, Directory(..)
) where

import Data.Word
import GHC.OverloadedLabels (IsLabel(..))
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Reader hiding (Reader)

-- | Base type for files
data File m = File
  { fileName        :: String
  , filePermissions :: Word16
  , fileOwner       :: String
  , fileGroup       :: String
  , fileRead        :: Maybe (Reader m)
  , fileWrite       :: Maybe (Writer m)
  , fileQIDPath     :: Word64
  }

instance Show (File m) where
  show File{..} = "<<" <> show fileQIDPath <> ":" <> fileName <> ">>"

-- | Base type for directories
data Directory m = Directory
  { dirName        :: String
  , dirOwner       :: String
  , dirGroup       :: String
  , dirPermissions :: Word16
  , dirQIDPath     :: Word64
  }

instance Show (Directory m) where
  show Directory{..} = "<<" <> show dirQIDPath <> ":" <> dirName <> ">>"

-- | Simple reader on lazy bytestrings
newtype Reader m = Reader (m B.ByteString)

-- | Simple writer on lazy bytestrings
newtype Writer m = Writer (B.ByteString -> m ())

data FileSystemFT m r = FileNode (File m) r
                      | DirNode  (Directory m) ~(m (FileSystemT m ())) r
                      | Embed (m r)
                      deriving Functor

data FileSystemT m a = Pure a
                     | Free ~(FileSystemFT m (FileSystemT m a))

instance Functor m => Functor (FileSystemT m) where
  fmap f (Pure a) = Pure $ f a
  fmap f (Free r) = Free $ fmap (fmap f) r

instance Monad m => Applicative (FileSystemT m) where
  pure = Pure
  (<*>) = ap

instance Monad m => Monad (FileSystemT m) where
  (Pure x) >>= f = f x
  (Free r) >>= f = Free (fmap (>>= f) r)

type FileSystem = FileSystemT IO

data SetterProxy a = SetterProxy


class Setter obj label t | obj label -> t where
  set :: SetterProxy label -> obj -> t -> obj

class Getter obj label t | obj label -> t where
  grab :: SetterProxy label -> obj -> t

instance l ~ x => IsLabel l (SetterProxy x) where
  fromLabel = SetterProxy

instance Setter (File m) "name"    String     where set _ o v = o {fileName        = v}
instance Setter (File m) "perms"   Word16     where set _ o v = o {filePermissions = v}
instance Setter (File m) "owner"   String     where set _ o v = o {fileOwner       = v}
instance Setter (File m) "group"   String     where set _ o v = o {fileGroup       = v}
instance Setter (File m) "read"    (Reader m) where set _ o v = o {fileRead        = Just v}
instance Setter (File m) "write"   (Writer m) where set _ o v = o {fileWrite       = Just v}
instance Setter (File m) "qidPath" Word64     where set _ o v = o {fileQIDPath     = v}

instance Setter (Directory m) "name"    String where set _ o v = o {dirName        = v}
instance Setter (Directory m) "perms"   Word16 where set _ o v = o {dirPermissions = v}
instance Setter (Directory m) "owner"   String where set _ o v = o {dirOwner       = v}
instance Setter (Directory m) "group"   String where set _ o v = o {dirGroup       = v}
instance Setter (Directory m) "qidPath" Word64 where set _ o v = o {dirQIDPath     = v}

instance Getter (File m) "name"    String     where grab _ = fileName
instance Getter (File m) "perms"   Word16     where grab _ = filePermissions
instance Getter (File m) "owner"   String     where grab _ = fileOwner
instance Getter (File m) "group"   String     where grab _ = fileGroup
instance Getter (File m) "qidPath" Word64     where grab _ = fileQIDPath
instance Getter (File m) "read"  (Maybe (Reader m)) where grab _ = fileRead
instance Getter (File m) "write" (Maybe (Writer m)) where grab _ = fileWrite

instance Getter (Directory m) "name"    String where grab _ = dirName
instance Getter (Directory m) "perms"   Word16 where grab _ = dirPermissions
instance Getter (Directory m) "owner"   String where grab _ = dirOwner
instance Getter (Directory m) "group"   String where grab _ = dirGroup
instance Getter (Directory m) "qidPath" Word64 where grab _ = dirQIDPath


emptyDir :: Monad m => Directory m
emptyDir = Directory
  { dirName        = ""
  , dirPermissions = 0o777
  , dirOwner       = "nobody"
  , dirGroup       = "nobody"
  , dirQIDPath     = 0
  }

emptyFile :: (Monad m) => File m
emptyFile = File
  { fileName        = ""
  , filePermissions = 0o554
  , fileOwner       = "nobody"
  , fileGroup       = "nobody"
  , fileRead        = Nothing
  , fileWrite       = Nothing
  , fileQIDPath     = 0
  }

-- | This is for setting properties on files and directories using @-XOverloadedLabels@
-- An example would be 
--
-- > file "foo" [#perms := 0o777]
data Attribute a where
  (:=) :: (IsLabel label (SetterProxy label), Setter obj label t) => SetterProxy label -> t -> Attribute obj


applyAttr :: obj -> Attribute obj -> obj
applyAttr obj (prop := v) = set prop obj v
{-# INLINE applyAttr #-}

applyAttrs :: obj -> [Attribute obj] -> obj
applyAttrs = foldl applyAttr
{-# INLINE applyAttrs #-}

class INodeDir arg result | result -> arg where
  iNodeDir :: String -> arg -> result

instance (Monad m, f ~ FileSystemT m ()) => INodeDir [Attribute (Directory m)] (f -> FileSystemT m ()) where
  iNodeDir name attrs = mkDir (#name := name : attrs)
  {-# INLINE iNodeDir #-}

instance (Monad m) => INodeDir (FileSystemT m ()) (FileSystemT m ()) where
  iNodeDir name = mkDir [#name := name]
  {-# INLINE iNodeDir #-}

class INodeFile m ty t | t -> m ty where
  iNodeFile :: [Attribute ty] -> t

instance (Monad m) => INodeFile m (File m) (FileSystemT m ()) where
  iNodeFile = mkFile
  {-# INLINE iNodeFile #-}

instance (Monad m, INodeFile m ty t, ty ~ File m) => INodeFile m ty ([Attribute ty] -> t) where
  iNodeFile xs attrs = iNodeFile (attrs <> xs)
  {-# INLINE iNodeFile #-}

instance (Monad m, INodeFile m ty t, ty ~ File m) => INodeFile m ty (String -> t) where
  iNodeFile xs name = iNodeFile $ (#name := name) : xs
  {-# INLINE iNodeFile #-}

instance (Monad m, INodeFile m ty t, ty ~ File m) => INodeFile m ty (Reader m -> t) where
  iNodeFile xs r = iNodeFile $ (#read := r) : xs
  {-# INLINE iNodeFile #-}

instance (Monad m, INodeFile m ty t, ty ~ File m) => INodeFile m ty (Writer m -> t) where
  iNodeFile xs w = iNodeFile $ (#write := w) : xs
  {-# INLINE iNodeFile #-}

class Ends e t
instance {-# OVERLAPPING #-} Ends e a => Ends e (b -> a)
instance {-# OVERLAPPABLE #-} e ~ a => Ends e (FileSystemT m a)

mkDir :: (Monad m) => [Attribute (Directory m)] -> FileSystemT m () -> FileSystemT m ()
mkDir attrs children = Free $ DirNode (applyAttrs emptyDir attrs) (pure children) $ pure ()

mkFile :: Monad m => [Attribute (File m)] -> FileSystemT m ()
mkFile attrs = Free $ FileNode (applyAttrs emptyFile attrs) $ pure ()

-- | A directory node in the file system
dir :: (INodeDir arg res) => String -> arg -> res
dir = iNodeDir
{-# INLINE dir #-}

-- | A File node in the file system
file :: (Monad m, INodeFile m (File m) t, Ends () t) => String -> t
file = iNodeFile []
{-# INLINE file #-}


instance MonadTrans FileSystemT where
  lift ma = Free (Embed (ma >>= \a -> return $ Pure a))

instance MonadState s m => MonadState s (FileSystemT m) where
  get = lift get
  put = lift . put
  state f = lift (state f)

instance MonadReader r m => MonadReader r (FileSystemT m) where
  ask = lift ask
  local f (Pure a) = Pure a
  local f (Free op) = Free $ case op of
    FileNode fi r -> FileNode fi (local f r)
    DirNode dir c r -> DirNode dir (local f c) (local f r)
    Embed xm  -> Embed (local f xm)
