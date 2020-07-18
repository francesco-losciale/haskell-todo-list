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


# How to run

- `libpq-dev` is required if you want to build the executable. It can be 
installed with `brew install postgresql`

- `stack build` if you want to build the executables

- `stack test` 

- `stack test --dry-run` to see all the possible test you can run

- `stack test --test-arguments "--match=SimpleState"` runs a specific test