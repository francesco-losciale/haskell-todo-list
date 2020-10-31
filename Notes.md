# Takeaways

- Paring with an haskeller seems to be the fastest way to learn idiomatic Haskell. Rely on the community if you can't.
- FP programmers tend to abstract more up front - see https://www.sitepoint.com/oop-learn-about-abstraction-from-fp/
- More domain modeling and lest TDD (stubbing instead of mocking is an advantage)
- Add test of properties such as associativity and distributivity if you are going to use algebraic data structure (example, monoids)
- Build small functions and compose them. Easier to adhere to OCP. 
- Small increments
- Continuous deployment/hotswapping: easier without state - see http://simonmar.github.io/posts/2017-10-17-hotswapping-haskell.html 
- Separation of behaviour from data comes naturally. Example: lifting of a function over a monad (example Maybe)
    ```
    Prelude> fmap (+1) $ Just 1
        Just 2
    Prelude> fmap (+1) [1, 2, 3]
        [2,3,4]
    ``` 
    In both cases, the function/behaviour lifted is the same. In the first case. It’s the context (functor) we’ve lifted the function over that determines how the function behaves with the data. 
    The datatype and the Functor instance determine the Context. 
    The context is also the only place where exception handling sits.
- OO makes code more cohesive. FP makes code more reusable. We tend to create private methods for a single purpose (the object logic) because of the state we need to manage. In FP functions are all public and reusable from anywhere, they are not bound to any other state/variable. (Neal Ford)
- FP abstractions are smaller. Easier to manage.
- Use always records instead of data constructor, it is easier to add or remove fields. 
- Tdd only after defining the domain model
- Follow yagni as much as possible when learning to avoid rabbit holes and waste of time
- Use [this](./CheatSheet.pdf) Haskell cheat sheet to review your code


