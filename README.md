# Haskell Todo List - work in progress


## How to run

Pre-requisites:

- To build, you need `libpq-dev` that can be installed with `brew install postgresql` (<-- can you improve this???)

Run:

- To run tests, you need Docker Machine

- Compile and build executables: `stack build` 

- Compile and run all tests: `stack test` 

- List the tests you can run individually: `stack test --dry-run` 

- Run an individual test: `stack test --test-arguments "--match=SimpleState"` 

- Run application `stack exec haskell-todo-list-exe`

- If Hlint is installed, run `hlint .` in the project root directory



## Learnings

- Notes about monads are here https://github.com/francesco-losciale/haskell-state-monad

- Some more [here](./Notes.md)

- Useful links and resources [here](./Links.md)



## Want to learn more

- Possible improvements to the code
    - implement Lens on Todo type
    - Use `Data.Cache` to temporarily cache the list without peristing it - https://hackage.haskell.org/package/cache-0.1.3.0/docs/Data-Cache.html
    - Learn STM and find a use case - curious to see how `retry` of a blocked transaction works
- Exercises: Haskell patterns minibook: https://kowainik.github.io/posts/haskell-mini-patterns <-- to learn idiomatic code
- Learn [Vavr](https://www.vavr.io/vavr-docs/) : Java FP, find use cases at work
