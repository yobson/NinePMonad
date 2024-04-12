{-# LANGUAGE GeneralizedNewtypeDeriving, FunctionalDependencies, DataKinds, GADTs,
             FlexibleContexts, TypeFamilies, AllowAmbiguousTypes, UndecidableInstances,
             FlexibleInstances, ScopedTypeVariables, PolyKinds, RankNTypes, OverloadedLabels, 
             RecordWildCards 
#-}

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

data Directory = Directory
  { dirName        :: String
  , dirOwner       :: String
  , dirGroup       :: String
  , dirPermissions :: Word16
  }

instance Show Directory where
  show Directory{..} = "<<" <> showOct dirPermissions "" <> ":" <> dirName <> ">>"

newtype Reader m = Reader (m B.ByteString)
newtype Writer m = Writer (B.ByteString -> m ())

data FileTree m a = Branch a Directory [FileTree m a]
                  | Leaf   a (File m)
                  deriving Show

getNode :: FileTree m a -> Either Directory (File m)
getNode (Leaf _ f)     = Right f
getNode (Branch _ d _) = Left d

newtype FileSystemT m a = FileSystemT { unFileSystem :: StateT [FileTree m ()] m a }
  deriving (Monad, Functor, Applicative, MonadFix)

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

getProp :: (IsLabel l (SetterProxy l), Getter (FileTree m a) l b) => SetterProxy l -> FileTree m a -> b
getProp = grab 

setProp :: (IsLabel l (SetterProxy l), Setter (FileTree m a) l b) => SetterProxy l -> FileTree m a -> b -> FileTree m a
setProp = set

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


dir :: (INodeDir arg res) => String -> arg -> res
dir = iNodeDir
{-# INLINE dir #-}

file :: (Monad m, INodeFile m (File m) t, Ends () t) => String -> t
file = iNodeFile []
{-# INLINE file #-}
