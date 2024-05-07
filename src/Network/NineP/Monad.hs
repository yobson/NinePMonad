{-# LANGUAGE GeneralizedNewtypeDeriving, FunctionalDependencies, DataKinds, GADTs,
             FlexibleContexts, TypeFamilies, AllowAmbiguousTypes, UndecidableInstances,
             FlexibleInstances, ScopedTypeVariables, PolyKinds, RankNTypes, OverloadedLabels, 
             RecordWildCards 
#-}

{-|
Module      : Network.NineP.Monad
Description : Monad for constructing file systems
Maintainer  : james@hobson.space
Copyright   : (c) James Hobson, 2024
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
, FileSystem
, Reader(..)
, Writer(..)
, runFileSystemT
, file
, dir
, Attribute(..)
, File(..)
, Directory(..)
, FileTree(..)
, getProp
, setProp
, modifyProp
) where

import Numeric
import Data.Word
import GHC.OverloadedLabels (IsLabel(..))
import Control.Monad.State.Strict
import qualified Data.ByteString.Lazy as B

-- | Base type for files
data File m = File
  { fileName        :: String
  , filePermissions :: Word16
  , fileOwner       :: String
  , fileGroup       :: String
  , fileRead        :: Maybe (Reader m)
  , fileWrite       :: Maybe (Writer m)
  }

instance Show (File m) where
  show File{..} = "<<" <> showOct filePermissions ":" <> fileName <> ">>"

-- | Base type for directories
data Directory = Directory
  { dirName        :: String
  , dirOwner       :: String
  , dirGroup       :: String
  , dirPermissions :: Word16
  }

instance Show Directory where
  show Directory{..} = "<<" <> showOct dirPermissions "" <> ":" <> dirName <> ">>"

-- | Simple reader on lazy bytestrings
newtype Reader m = Reader (m B.ByteString)

-- | Simple writer on lazy bytestrings
newtype Writer m = Writer (B.ByteString -> m ())

-- | A file system represented as a rose tree. 
data FileTree m a = Branch a Directory [FileTree m a]
                  | Leaf   a (File m)
                  deriving Show

getNode :: FileTree m a -> Either Directory (File m)
getNode (Leaf _ f)     = Right f
getNode (Branch _ d _) = Left d

-- | File system monad type
newtype FileSystemT m a = FileSystemT { unFileSystem :: StateT [FileTree m ()] m a }
  deriving (Monad, Functor, Applicative, MonadFix)

-- | Convert the filesystem monad into a rosetree
runFileSystemT :: (Monad m) => FileSystemT m a -> m (a, [FileTree m ()])
runFileSystemT fs = runStateT (unFileSystem fs) []

type FileSystem = FileSystemT IO

data SetterProxy a = SetterProxy


class Setter obj label t | obj label -> t where
  set :: SetterProxy label -> obj -> t -> obj

class Getter obj label t | obj label -> t where
  grab :: SetterProxy label -> obj -> t

instance l ~ x => IsLabel l (SetterProxy x) where
  fromLabel = SetterProxy

instance Setter (File m) "name"  String     where set _ o v = o {fileName        = v}
instance Setter (File m) "perms" Word16     where set _ o v = o {filePermissions = v}
instance Setter (File m) "owner" String     where set _ o v = o {fileOwner       = v}
instance Setter (File m) "group" String     where set _ o v = o {fileGroup       = v}
instance Setter (File m) "read"  (Reader m) where set _ o v = o {fileRead        = Just v}
instance Setter (File m) "write" (Writer m) where set _ o v = o {fileWrite       = Just v}

instance Setter Directory "name"  String where set _ o v = o {dirName        = v}
instance Setter Directory "perms" Word16 where set _ o v = o {dirPermissions = v}
instance Setter Directory "owner" String where set _ o v = o {dirOwner       = v}
instance Setter Directory "group" String where set _ o v = o {dirGroup       = v}

instance Getter (File m) "name"  String     where grab _ = fileName
instance Getter (File m) "perms" Word16     where grab _ = filePermissions
instance Getter (File m) "owner" String     where grab _ = fileOwner
instance Getter (File m) "group" String     where grab _ = fileGroup
instance Getter (File m) "read"  (Maybe (Reader m)) where grab _ = fileRead
instance Getter (File m) "write" (Maybe (Writer m)) where grab _ = fileWrite

instance Getter Directory "name"  String where grab _ = dirName
instance Getter Directory "perms" Word16 where grab _ = dirPermissions
instance Getter Directory "owner" String where grab _ = dirOwner
instance Getter Directory "group" String where grab _ = dirGroup

instance Getter (FileTree m a) "name"  String where grab l = either (grab l) (grab l) . getNode
instance Getter (FileTree m a) "perms" Word16 where grab l = either (grab l) (grab l) . getNode
instance Getter (FileTree m a) "owner" String where grab l = either (grab l) (grab l) . getNode
instance Getter (FileTree m a) "group" String where grab l = either (grab l) (grab l) . getNode

-- | Get a property from the root of a file tree
getProp :: (IsLabel l (SetterProxy l), Getter (FileTree m a) l b) => SetterProxy l -> FileTree m a -> b
getProp = grab 

-- | Set a property from the root of a file tree
setProp :: (IsLabel l (SetterProxy l), Setter (FileTree m a) l b) => SetterProxy l -> FileTree m a -> b -> FileTree m a
setProp = set

-- | Modify a property from the root of a file tree
modifyProp :: (IsLabel l (SetterProxy l), Getter (FileTree m a) l b, Setter (FileTree m a) l b) => SetterProxy l -> FileTree m a -> (b -> b) -> FileTree m a
modifyProp l ft f = setProp l ft $ f $ getProp l ft

instance (Monad m) => MonadState [FileTree m ()] (FileSystemT m) where
  get = FileSystemT get
  put new = FileSystemT $ StateT $ \_ -> pure ((), new)

instance (MonadIO m) => MonadIO (FileSystemT m) where
  liftIO xm = FileSystemT $ liftIO xm

emptyDir :: Directory
emptyDir = Directory
  { dirName        = ""
  , dirPermissions = 0o777
  , dirOwner       = "root"
  , dirGroup       = "root"
  }

emptyFile :: (Monad m) => File m
emptyFile = File
  { fileName        = ""
  , filePermissions = 0o554
  , fileOwner       = "root"
  , fileGroup       = "root"
  , fileRead        = Nothing
  , fileWrite       = Nothing
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

instance (Monad m, f ~ FileSystemT m a) => INodeDir [Attribute Directory] (f -> FileSystemT m a) where
  iNodeDir name attrs = mkDir (#name := name : attrs)
  {-# INLINE iNodeDir #-}

instance (Monad m) => INodeDir (FileSystemT m a) (FileSystemT m a) where
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

mkDir :: (Monad m) => [Attribute Directory] -> FileSystemT m a -> FileSystemT m a
mkDir attrs children = do
  save <- get
  put []
  v <- children
  st <- get
  put save
  let node = applyAttrs emptyDir attrs
  modify (Branch () node st : )
  return v

mkFile :: Monad m => [Attribute (File m)] -> FileSystemT m ()
mkFile attrs = modify ((Leaf () $ applyAttrs emptyFile attrs) :)
{-# INLINE mkFile #-}

-- | A directory node in the file system
dir :: (INodeDir arg res) => String -> arg -> res
dir = iNodeDir
{-# INLINE dir #-}

-- | A File node in the file system
file :: (Monad m, INodeFile m (File m) t, Ends () t) => String -> t
file = iNodeFile []
{-# INLINE file #-}
