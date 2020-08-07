# Haskell Todo List - work in progress

Side project to test Haskell learnings.

As part of this project, the most significant solved problems are: 

- To learn Monads, the State monad has been rewritten from scratch following these steps:

    1. Firstly, implemented an alias of a state transformer function - see `State.SimpleState`.
    2. Secondly, replaced the alias with a constructor and added the accessor function to apply the state transformer out of the monad - see `State.State`. Only `runState` needs to be exported now.
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


# TODOs & Open points

- Review/refactor current unit tests
- Refactoring & Improvements of Todo code
- How to provide specific error types to the user? Learn Either
- Learn Lens - watch talk from Pawel Szulc on Lenses - https://vimeo.com/user119686798
- Use `Data.Cache` to temporarily save list without peristing it - https://hackage.haskell.org/package/cache-0.1.3.0/docs/Data-Cache.html
- Can you see any use case for STM?
- Can you see any use case for Concurrency?
- Try to bring FP in Java refactoring real production code

# Thoughts & takes

- To learn Haskell is better to have multiple sources & iterate more on the same topics
- FP programmers tend to abstract more than OO - see https://www.sitepoint.com/oop-learn-about-abstraction-from-fp/
- Monoid formal definition: https://github.com/francesco-losciale/haskell-annotations
- 



----------- 
Adjust following text

Notes

We write tons of unit tests for Controller and Service classes, which are actually just functions and delegators. With FP can we just have integration tests and stubbing instead of mocking for all these layers?

Spec tests (Hspec) Unit tests (HUnit) Property tests (QuickCheck - This relies on the type system to know what kinds of data to generate.) -test laws of monads or basic associativity

TDD: outside-in or inside-out a new approach: infrastructure first, business logic after with contract testing this could be parallel

I would never use Haskell, it's strongly typed and even adding information to a data structure doesn't seem easy. Data types need to be necessarily thought up front.

Things you can use now:

destructuring

partial function

equational reasoning (you need high order function)

DDD & lazy evaluation: an expression is evaluated depending on the context

is fp the real agile programmning technique? we only increment & compose code , never change or refactor existing one. Also continuous deployment (hot code replacement)

concurrency http://www.defmacro.org/2006/06/19/fp.html

in OO we raise exception and handle them in a wider context. this is fundamentally wrong. failure scenario should be handled internally in the structure. if we want to capture them to enable other side effects (such as logging) we want to add this behaviour to the piece throwing exception (something aspect oriented programming tried to do).

Functional programming forces you to follow baby steps. Taking advantages of REPL and of tests (stubbing values, not mocking behaviour)

DDD we lift a function in the Maybe context. p. 667
Prelude> fmap (+1) $ Just 1
     Just 2
     Prelude> fmap (+1) [1, 2, 3]
     [2,3,4]
In both cases, the function we’re lifting is the same. In the first case, we lift that function into a Maybe context in order to apply it; in the second case, into a list context. It can be helpful to think of it in terms of lifting the function into the context, because it’s the context we’ve lifted the function into that determines how the function will get applied (to just one value or recursively to many, for example). The context is the datatype, the def- inition of the datatype, and the Functor instance we have for that datatype. It’s also the contexts that determine what happens when we try to apply a function to an 𝑎 that isn’t there:
Others:

FP first time I got interested with a monoid exercise in java- see utility and capture motivation so that people can learn more by themselves https://twitter.com/ntcoding/status/1251417405953314817?s=19

is it possible to change rule in mint to be a monoid?