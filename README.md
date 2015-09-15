# Installing

You will need to install ghc and cabal first. I have only tried to run typegen on MacOSX although I dont see why it wouldnt work on any other os. Probably wont work on Windows because of paths... I dont know, I just dont trust Windows.

I recommend using a cabal sandbox although its not necessary.  

> cabal sandbox init

You will need to install happy first. Its necessary for hoogle although I dont know why you need to install it separately.

> cabal install happy --enable-library-profiling \
                      --enable-executable-profiling \
                      --enable-tests \
                      --enable-benchmarks

It is very important to install the modified version of hoogle that comes with typegen.

> cabal install hoogle-4.2.41/ --enable-library-profiling \
                               --enable-executable-profiling \
                               --enable-tests \
                               --enable-benchmarks

Make sure you include that forward slash after hoogle otherwise it will try to search hackage for that version of hoogle, we want it to install from that folder.

Then install the other dependencies.

> cabal install --only-dependencies \
                --enable-library-profiling \
                --enable-executable-profiling \
                --enable-tests \
                --enable-benchmarks

Then you can run it with:

> cabal run
