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
>   file "file1" (StringR $ return "Hello world")
>   file "file2" (StringW $ putStrLn)
-}

module Network.NineP.Monad
( FileSystemT(..)
, FileSystemFT(..)
, FileSystem
, RawReader(..)
, RawWriter(..)
, StringReader(..)
, StringWriter(..)
, file
, dir
, getProp
, Attribute(..)
, File(..)
, Directory(..)
) where

import Data.Word
import GHC.OverloadedLabels (IsLabel(..))
import Control.Monad
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Reader hiding (Reader)

-- | Base type for files
data File m = File
  { fileName        :: String
  , filePermissions :: Word16
  , fileOwner       :: String
  , fileGroup       :: String
  , fileRead        :: Maybe (RawReader m)
  , fileWrite       :: Maybe (RawWriter m)
  }

instance Show (File m) where
  show File{..} = "<<" <> fileName <> ">>"

-- | Base type for directories
data Directory m = Directory
  { dirName        :: String
  , dirOwner       :: String
  , dirGroup       :: String
  , dirPermissions :: Word16
  }

instance Show (Directory m) where
  show Directory{..} = "<<" <> ":" <> dirName <> ">>"

-- | Raw reader on lazy bytestrings
newtype RawReader m = RawReader
  (  Word64 -- ^ Offset
  -> Word32 -- ^ Bytes Request
  -> m B.ByteString
  )

-- | Raw writer on lazy bytestrings
newtype RawWriter m = RawWriter 
  (  Word64 -- ^ Offset
  -> B.ByteString 
  -> m ()
  )

-- | Raw reader on lazy bytestrings
newtype StringReader m = StringR (m String)

-- | Raw writer on lazy bytestrings
newtype StringWriter m = StringW (String -> m ())

string2RawReader :: Monad m => StringReader m -> RawReader m
string2RawReader (StringR f) = RawReader $ \offset count -> do
  if offset == 0 
    then do
      cont <- f
      return $ B.take (fromIntegral count) $ B.drop (fromIntegral offset) $ BC.pack cont
    else return B.empty

string2RawWriter :: Monad m => StringWriter m -> RawWriter m
string2RawWriter (StringW f) = RawWriter $ \_ -> f . BC.unpack

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

instance Setter (File m) "name"    String        where set _ o v = o {fileName        = v}
instance Setter (File m) "perms"   Word16        where set _ o v = o {filePermissions = v}
instance Setter (File m) "owner"   String        where set _ o v = o {fileOwner       = v}
instance Setter (File m) "group"   String        where set _ o v = o {fileGroup       = v}
instance Setter (File m) "read"    (RawReader m) where set _ o v = o {fileRead        = Just v}
instance Setter (File m) "write"   (RawWriter m) where set _ o v = o {fileWrite       = Just v}

instance Setter (Directory m) "name"    String where set _ o v = o {dirName        = v}
instance Setter (Directory m) "perms"   Word16 where set _ o v = o {dirPermissions = v}
instance Setter (Directory m) "owner"   String where set _ o v = o {dirOwner       = v}
instance Setter (Directory m) "group"   String where set _ o v = o {dirGroup       = v}

instance Getter (File m) "name"    String     where grab _ = fileName
instance Getter (File m) "perms"   Word16     where grab _ = filePermissions
instance Getter (File m) "owner"   String     where grab _ = fileOwner
instance Getter (File m) "group"   String     where grab _ = fileGroup
instance Getter (File m) "read"  (Maybe (RawReader m)) where grab _ = fileRead
instance Getter (File m) "write" (Maybe (RawWriter m)) where grab _ = fileWrite

instance Getter (Directory m) "name"    String where grab _ = dirName
instance Getter (Directory m) "perms"   Word16 where grab _ = dirPermissions
instance Getter (Directory m) "owner"   String where grab _ = dirOwner
instance Getter (Directory m) "group"   String where grab _ = dirGroup

instance Getter (Either (File m) (Directory m)) "name"    String where grab _ = either fileName dirName
instance Getter (Either (File m) (Directory m)) "perms"   Word16 where grab _ = either filePermissions dirPermissions
instance Getter (Either (File m) (Directory m)) "owner"   String where grab _ = either fileOwner dirOwner
instance Getter (Either (File m) (Directory m)) "group"   String where grab _ = either fileGroup dirGroup


getProp :: (IsLabel l (SetterProxy l), Getter obj l b) => SetterProxy l -> obj -> b
getProp = grab 


emptyDir :: Monad m => Directory m
emptyDir = Directory
  { dirName        = ""
  , dirPermissions = 0o777
  , dirOwner       = "nobody"
  , dirGroup       = "nobody"
  }

emptyFile :: (Monad m) => File m
emptyFile = File
  { fileName        = ""
  , filePermissions = 0o554
  , fileOwner       = "nobody"
  , fileGroup       = "nobody"
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

instance (Monad m, INodeFile m ty t, ty ~ File m) => INodeFile m ty (RawReader m -> t) where
  iNodeFile xs r = iNodeFile $ (#read := r) : xs
  {-# INLINE iNodeFile #-}

instance (Monad m, INodeFile m ty t, ty ~ File m) => INodeFile m ty (RawWriter m -> t) where
  iNodeFile xs w = iNodeFile $ (#write := w) : xs
  {-# INLINE iNodeFile #-}

instance (Monad m, INodeFile m ty t, ty ~ File m) => INodeFile m ty (StringReader m -> t) where
  iNodeFile xs r = iNodeFile $ (#read := string2RawReader r) : xs
  {-# INLINE iNodeFile #-}

instance (Monad m, INodeFile m ty t, ty ~ File m) => INodeFile m ty (StringWriter m -> t) where
  iNodeFile xs w = iNodeFile $ (#write := string2RawWriter w) : xs
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
  local _ (Pure a) = Pure a
  local f (Free op) = Free $ case op of
    FileNode fi r -> FileNode fi (local f r)
    DirNode di c r -> DirNode di (local f c) (local f r)
    Embed xm  -> Embed (local f xm)
