-- one database, multiple schemas

CREATE SCHEMA schema_todo AUTHORIZATION postgres; -- create schema authorized for user `postgres`

-- There is a PostgreSQL environment variable "search_path". If you set the value
-- of this variable to "x,a,public" then PostgreSQL will look for all the tables,
-- types and function names in the schema "x". If there is no such table in this schema,
-- then it will look for this table in the next schema, which is "a" in this example.

ALTER ROLE postgres SET search_path TO schema_todo;

CREATE TABLE schema_todo.todo_list (
  description TEXT PRIMARY KEY,
  status VARCHAR
)