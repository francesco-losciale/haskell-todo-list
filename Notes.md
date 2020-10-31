# Takeaways

- FP programmers abstract more up front - see https://www.sitepoint.com/oop-learn-about-abstraction-from-fp/
- FP better testable (stubbing over mocking)
- Test complexity by testing properties such as associativity and distributivity - if you can abstract using algebraic data structure (monoids, monads)
- SOLID principle seems to adhere more to FP than OOP
- Small functions, small increments - baby step
- OO tries to make code around a concept cohesive. FP tries to make code more reusable. We tend to create private methods for a single purpose (the object logic) because of the state we need to manage. In FP functions are all public and reusable from anywhere, they are not bound to any other state/variable. (Neal Ford)
- Composition (of functions) vs inherithance (coupling)
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
- Haskell: always records instead of data constructor, it is easier to add or remove fields. 
- Tdd only after defining the domain model
- Use [this](./CheatSheet.pdf) Haskell cheat sheet to review your code


