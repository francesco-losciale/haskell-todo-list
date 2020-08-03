# haskell-todo-list

TODO: 

- manage serialisation
- add transactionality

- add web stack: https://williamyaoh.com/posts/2019-11-16-a-dead-simple-web-stack.html
   -- http://happstack.com/page/view-page-slug/9/happstack-lite-tutorial
   -- http://www.serpentine.com/wreq/tutorial.html
   
Questions:

- How to provide specific error types to the user?
- how transactionality works: for example : `createPerson . saveDatabase . sendQueue`
    if one function fails I want to rollback. 
    ScottW suggested compensating transactions, with undoFunctions to be passed forward.
    Is there a better way?
- retry mechanism?


# State examples

In the repositories it's possible to find three examples of a State handling in Haskell, 
developed for learning purporses.

The first to look at is SimpleState that is a synonym of a state transformer. Since it is a synonym,
it's easier to understand how the state works, because the data constructor is not needed.

Second one is State, same thing with a proper newtype

Third is the StateMonad, with typeclasses implementation of Functor, Applicative and Monad. 
At this point it's possible to use the code in a do-block because of syntactic sugar from the compiler (>>=)

# How to run

- `libpq-dev` is required if you want to build the executable. It can be 
installed with `brew install postgresql`

- `stack build` if you want to build the executables

- `stack test` 

- `stack test --dry-run` to see all the possible test you can run

- `stack test --test-arguments "--match=SimpleState"` runs a specific test


 To run hlint, cd to haskell-todo-list and run `hlint .`