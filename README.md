# NinePMonad

This is a library for making [9p](https://en.wikipedia.org/wiki/9P_(protocol))(2000) file systems in haskell.
In short, you make file systems that are mountable locally, or over TCP. Reading and writing to the files
will invoke functions in your haskell app!

This can make distributed computing easier, it can make plug-in systems language agnostic and so on and so on.

## How to use?

To include this in your project, use a `cabal.project` file. This is because `freer-simple` seems to be
temorarily unmaintained and I am using a fork over git. You can see how [here](https://cabal.readthedocs.io/en/latest/how-to-source-packages.html#setting-up-a-source-code-dependency)
Then you can include `NinePMonad` in your `build-depends` and you are good to go!

Everything you need is in `Network.NineP`


## The FileSystem Monad

You can define file systemd with `FileSystem ()` using `dir` and `file`. All file sysytems must have a single
root directory node, "/":
```haskell
myFS :: FileSystem ()
myFS = dir "/" $ return ()
```

Nodes can have attributes given to them, owner, group and permissions. We can set the owner of our root directory
like this:
```haskell
myFS :: FileSystem ()
myFS = dir "/" [#owner := "me"] $ return ()
```

We use `do` notation to add children:
```haskell
myFS :: FileSystem ()
myFS = dir "/" [#owner := "me"] $ do
    file "hello"
    file "hello2" [#perms := 0o777]
    dir "some-folder" $ 
        return () -- empty
```

Finally we can attach read and write handles to files:
```haskell
myFS :: FileSystem ()
myFS = dir "/" [#owner := "me"] $ do
    file "hello" (StringR $ return "Hello World")
    file "hello2" [#perms := 0o777]
    dir "some-folder" $ 
        return () -- empty
```

The full list of attributes for files and directories is:

- perms
- owner
- group

The kinds of readers and writers are:

- RawReader :: Word64 {- Offset -} -> Word32 {- Bytes requested -} -> m Bytestring
- RawWriter :: Word64 {- Offset -} -> Bytestring -> m ()
- StringR :: m String
- StringW :: String -> m ()


You can then run your server with:
```haskell
main = serveFileSystem (defaultConf "tcp!127.0.0.1!8000") myFS
```
Note the rather alian way of specifying the serve address. This comes from plan9! If you want to instead create a
unix socket, write `"unix!/path/to/socket"`.

## The FileSystem Monad Transformer

See `app/Main.hs` for example of how to use the FileSystem monad transformer
