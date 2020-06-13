# haskell-todo-list

1. starting from todomvc, define API and implement service (use http://todomvc.com/examples/vanilla-es6/)
    x- three views : Active, Completed, All (=Active+Completed) - filter
    x- action add an element (default status Active) - add operation on list
    x- action flag element as Completed - fmap
    x- action clear completed - filter
    - failures: validation errors
    - add web stack: https://williamyaoh.com/posts/2019-11-16-a-dead-simple-web-stack.html
   
Questions:

- How to provide specific error types to the user?
- how transactionality works: for example : `createPerson . saveDatabase . sendQueue`
    if one function fails I want to rollback. 
    ScottW suggested compensating transactions, with undoFunctions to be passed forward.
    Is there a better way?
- retry mechanism?
