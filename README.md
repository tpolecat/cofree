## Pure Functional Database Programming with Fixpoint Types

Here are the [slides](http://tpolecat.github.io/presentations/sw2016/slides.html) and [video](https://www.youtube.com/watch?v=7xSfLPD6tiQ) from Scala World 2016. Press `?` for help with the slides.

#### Compiling and Running the Companion Code

This code uses the unreleased `doobie-tsql` library, which checks SQL literals at compile time. The build assumes a live Postgres database with a `postgres` user with no password and a `prof` database. If you wish to use a different user you can edit the settings in `build-tsql.sbt` and `Cofree.scala`.

    psql -c 'create user postgres createdb'
    psql -c 'create database prof' -U postgres
    psql -c '\i create.sql' -d prof -U postgres

#### Compiling the Slides

If you wish to build the slides for some reason, type `tut` at the sbt prompt. The output will apppear in the `tut-out/` directory.
