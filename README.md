Functional Programming Course at TU Vienna, WS16

Brief list of contents:

1. Official problem statements in `docs`
2. Official tests for each exercise in `results`
3. My solutions and my tests of those solutions in `src` and `test`

This project works both on Hugs and GHCi

Assuming you have GHCi, cabal and HSpec, a quick start would look like:

```
cabal update
cabal configure --enable-tests
cabal build
cabal test
```

Note: looks like Haskell setup changed on Archlinux, the configure
step now looks like this:

```
cabal configure --disable-library-vanilla --enable-shared --enable-executable-dynamic
```

Part 1 of Task 8 was formulated in a very poor fashion and generally
was not worth the trouble (too few points, too much effort and second
guessing), otherwise you should be all set to go.