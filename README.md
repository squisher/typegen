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

# Running

Then you can run it with:

> cabal run

# Documentation

You can access some pretty sweet Haddock documentation by running:

> cabal haddock

Then opening the ./dist/doc/html/typegen/index.html page in your web browser.

I tried to add documentation to every visible data type and function in the code.

# Other very important info

This was not yet built to be a stand-alone binary.  

It is not quite production ready, which means that you still need to use it like a library.  

# Important Functions and Types

> GenProg.runGenProg :: Individual -> [Test] -> IO ()

This is the "_main_" function it takes a starting individual and a set of tests and runs the genetic program on that individual forever (I haven't gotten it to work perfectly so this is mostly just testing).

> Types.Value.Value, Types.Type.Type, Types.TestFunction.TestFunction

It is important to note about these types that they have implementations for `IsString` that allow you to read Haskell code and turn them into what they represent. These are really just convenience so that when you are testing you can treat strings as the values they represent.
