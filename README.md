
![Haskell logo](https://haskell-lang.org/static/img/logo.png?etag=rJR84DMh)

# Project Euler solutions in Haskell

[Project Euler](https://projecteuler.net/about) is 
a [popular list of programming problems](http://projecteuler.net/problems).
It was created in 2001 by Colin Hughes, 
and named after the famous mathematician [Leonhard Euler](https://en.wikipedia.org/wiki/Leonhard_Euler).

The problem set varies in difficulty and it is very mathematical in its nature. 
Due to this mathematical form I used haskell for solving them, 
it gives a chance to use pure functional programming.  
Haskell also plays very well with mathematical objects such as 
fractions, infinite integers, infinite lists, lambda calculus...

## Loading solution from GHCi

Start GHCi from the `src` directory, load the problem that you want and call the solution without arguments from the haskell interpreter.

      $ cd src 
      $ ghci
      Prelude> :l e001.hs
      [1 of 1] Compiling Main             ( e001.hs, interpreted )
      Ok, one module loaded.
      *Main> solution
        233168
      
