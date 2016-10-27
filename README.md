# Pure Functional Database Programming with Fixpoint Types

Companion code for the talk.

- Slides are published [here](http://tpolecat.github.io/presentations/cofree/slides.html) ... press `?` for help.

This code uses the unreleased compile-time query checking, which assumes a live Postgres database with a `postgres` user with no password.

```
psql -c 'create user postgres createdb'
psql -c 'create database prof' -U postgres
psql -c '\i create.sql' -d prof -U postgres
```

- If you wish to build the slides for some reason, type `tut` at the sbt prompt.
