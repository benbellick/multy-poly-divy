# Multivariate Polynomial Division Calculator

This is a simple repository implementing multivariate polynomial division as described in the book Ideals, Varietes, and Algorithms[^1] by Cox, Little, and O'Shea. 

I implemented it originally just to make sure I understood the algorithm, but I also wanted to see how difficult it would be to spin up a small website from an OCaml codebase. 

I may continue to expand and add more algorithms as I see fit while continuing to read. I hope that this is useful both for those interested in learning about the algorithm and using the calculator, as well as people interested in learning how to spin out OCaml libs into small demo websites!


## Building
Install OCaml and everything into your opam switch as usual. Also install the js dependencies via `yarn install` (ensure yarn is available). Then, `dune build` will build the project, and `yarn server` will serve it on localhost. 



[^1]:https://link.springer.com/book/10.1007/978-3-319-16721-3
