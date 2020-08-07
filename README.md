# Haskell Todo List - work in progress

Side project to test Haskell learnings

As part of this project, the following significant problems have been solved: 

- To learn Monads, the State monad has been rewritten from scratch:

    1. Firstly, implemented an alias of a state transformer function - see `State.SimpleState`.
    2. Secondly, replaced the alias with a constructor and added the accessor function to apply the state transformer out of the monad - see `State.State`.
    3. Finally, created the Monad instance for State, reusing the bind and the return functions. This State monad can be used in a do block - see `State`.StateMonad`
    4. Also, implemented a state monad with a transactional state inside - `see TransactionalStateMonad`.

- Implemented a back-end web stack application using the following libraries:
    - hspec
    - hspec-discover
    - postgresql-simple
    - happstack-server
    - happstack-lite (<-- can this be removed???)
    - wreq
    - lens
    - utf8-string
    - aeson


# How to run

Pre-requisites:

- To build, you need `libpq-dev` that can be installed with `brew install postgresql` (<-- can you improve this???)
- To run tests, you need Docker Machine


- Compile and build executables: `stack build` 

- Compile and run all tests: `stack test` 

- List the tests you can run individually: `stack test --dry-run` 

- Run an individual test: `stack test --test-arguments "--match=SimpleState"` 

- If Hlint is installed, run `hlint .` in the project root directory


# TODO & Open points

- Modules should export only specific name.
- How to provide specific error types to the user? Learn Either
- Use `Data.Cache` to temporarily save list without peristing it
- Can you see any use case for STM?
- Can you see any use case for Concurrency?
