# Haskell Todo List - work in progress

# How to run

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


# TODOs & Open points
- Rearrange links:
    https://hackage.haskell.org/package/wreq-0.5.3.2/docs/Network-Wreq.html
    Crash course on Happstack - http://happstack.com/docs/crashcourse/index.html
    https://stackoverflow.com/questions/35592415/multiple-monads-in-one-do-block
- Refactoring & Improvements of Todo code
    - ~~todo item validation can fail for more than one errors~~
    - ~~compare the two solutions, add inline comments~~
    - implement Lens on TodoItem
- Learn Lens https://hackage.haskell.org/package/lens-tutorial-1.0.4/docs/Control-Lens-Tutorial.html - watch talk from Pawel Szulc on Lenses - https://vimeo.com/user119686798 use lens in TodoValidation
- Go through Haskell cheat sheet and review code
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
- Initially I thought ControllerSpec such as unit test of webserver, 
but after having completed it I tought to move database tests in there
and make it an integration test. It makes more sense to test "controllers" and "database" as part of an integration test.
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
- use records instead of data constructor, so that it is easier to refactor adding or removing fields. 
- tdd only after having defined your model
- follow yagni and try to write as little code as possible, this would avoid you rabbit holes and waste of time
- Feeling: I went with the same confidence of java, rabbit hole