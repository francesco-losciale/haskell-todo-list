# Haskell Todo List - work in progress

Side project to test Haskell learnings.

What I learned so far: 

- To learn Monads, the State monad has been rewritten from scratch following these steps:

    1. Firstly, implemented an alias of a state transformer function - see `State.SimpleState`.
    2. Secondly, replaced the alias with a constructor and added the accessor function to apply the state transformer out of the monad - see `State.State`. Only `runState` needs to be exported now.
    3. Finally, created the Monad instance for State, reusing the bind and the return functions. This State monad can be used in a do block - see `State`.StateMonad`
    4. Also, implemented a state monad with a transactional state inside that provides commit/rollback actions on the state - `see TransactionalStateMonad`.

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

- Monad formal definition available [here](./Monad.md)

# How to run

Pre-requisites:

- To build, you need `libpq-dev` that can be installed with `brew install postgresql` (<-- can you improve this???)

Run:

- To run tests, you need Docker Machine


- Compile and build executables: `stack build` 

- Compile and run all tests: `stack test` 

- List the tests you can run individually: `stack test --dry-run` 

- Run an individual test: `stack test --test-arguments "--match=SimpleState"` 

- If Hlint is installed, run `hlint .` in the project root directory


# TODOs & Open points

- Refactoring & Improvements of Todo code
    - ~~todo item validation can fail for more than one errors~~
    - when there's at least one error at validation, you don't throw an exception (that is more system-related instead of domain code). Produce an Either list with errors instead and let the consumer of Todo deal with it.
    - the excepton should provide all the necessary information to the client
- Learn Lens - watch talk from Pawel Szulc on Lenses - https://vimeo.com/user119686798
- Use `Data.Cache` to temporarily save list without peristing it - https://hackage.haskell.org/package/cache-0.1.3.0/docs/Data-Cache.html
- Exercises: Haskell patterns minibook: https://kowainik.github.io/posts/haskell-mini-patterns <-- to learn idiomatic code
- Can you see any use case for STM? Katas?
- Can you see any use case for Concurrency? Katas?
- Try to bring FP in Java refactoring real production code


# Thoughts & takes

- To learn Haskell is better to have multiple sources & iterate more on the same topics. It takes time to absorb concepts. Pairing with a more experienced haskeller would be great to learn idioms that are not available on books, but on certain things you would need to write down notes to come back later on your own without time constraints.
- FP programmers tend to abstract more than OO - see https://www.sitepoint.com/oop-learn-about-abstraction-from-fp/
- The type system and the nature of functions seems to make FP code quicker to be tested compared to OO & necessary machinery it implies (mocking)
- When using algebras in Haskell, you must prove properties such as associativity and distributivity by yourself.
- Data types can change easily? Do we need to much up front data modeling? 
- How flexible/agile is Haskell code? If functions are extremely small, we can take advantage on composition. We could prefer function replacement to refactoring. We would only increment code to change behaviour?
- Continuous deployment/hotswapping: easier with FP - see http://simonmar.github.io/posts/2017-10-17-hotswapping-haskell.html 
- DDD-related: lifting a function over the Maybe context:
    ```
    Prelude> fmap (+1) $ Just 1
        Just 2
        Prelude> fmap (+1) [1, 2, 3]
        [2,3,4]
    ``` 
    In both cases, the function/behaviour lifted is the same. In the first case. It’s the context we’ve lifted the function over that determines how the function behaves with the data. The datatype and the Functor instance determine the Context. Further, ideally the context is also the only place where exception handling sits.
