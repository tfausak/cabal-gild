This is a Haskell repository that defines a command line executable for formatting Cabal files.

# Development flow

- Before running any other command, first run `cabal-gild --io cabal-gild.cabal`.
- To build the project, run `cabal build`.
- To run the test suite, run `cabal test`.
- To reformat a changed file, run `ormolu -i FILE`.
- To lint the project, run `hlint source`.

# Required before committing

- The build must succeed.
- The test suite must pass.
- Every file must be lint clean.
- Every file must be formatted.
