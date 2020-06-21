# haskell-todo-list


- add web stack: https://williamyaoh.com/posts/2019-11-16-a-dead-simple-web-stack.html
   
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