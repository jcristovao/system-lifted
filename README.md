system-lifted
=============

Lifted versions of Haskell System functions.

While haskell promotes the use of ```Maybe``` and ```Either``` as smart ways of dealing with errors, I found that the 
support for their Monad Transformers counter-parts is less than stellar.

While some packages like [Errors](http://hackage.haskell.org/package/errors-1.4.5/docs/Control-Error-Util.html) simplify
this, they still add a lot of boilerplate to the code.

The goal of this project started out as a way to write cleaner directory related code in either one of the _error related_
monad transformers, namely ```EitherT```, ```ErrorT``` or ```MaybeT``` (non-determinism and ```ListT``` are not yet supported).

Thus, by simply declaring some simple _template haskell_ at the start of a file:

```
type EitherIOText       = EitherT Text

deriveSystemLiftedErrors "DisallowIOE [HardwareFault]" ''EitherIOText
deriveSystemDirectory   ''EitherIOText
```

One could then write code like this:

```
getXdgConfigFolder :: EitherT IOException IO FilePath
getXdgConfigFolder = isRW =<< getEnv "XDG_CONFIG_HOME"
```

Currently the ```System.Directory``` and ```System.Environment``` are fully supported, and there is partial support for 
```System.Unix.Users```.
