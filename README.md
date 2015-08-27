# Installing

I recommend using a cabal sandbox although its not necessary.  

> cabal sandbox init

It is very important to install the modified version of hoogle that comes with typegen.

> cabal install hoogle-x.x.x/  --enable-library-profiling --enable-executable-profiling --enable-tests --enable-benchmarks

Then install the other dependencies.

> cabal install --only-dependencies --enable-library-profiling --enable-executable-profiling --enable-tests --enable-benchmarks

Then you can run it with:

> cabal run
