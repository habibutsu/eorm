# eorm

Object-Relational Mapping for work with Postgresql, inspired by http://sqlkorma.com/

## Motivation

* Lightweight and transparent way to build sql

* Minimization of mistakes and huge reduction of code at works with db

* Declarative description of mapping object attributes to db fields

* Supports partitioning ( *Not supported* but can be implemented through callbacks) **TODO**

* Supports sharding ( *Not supported* but can be implemented through callbacks) **TODO**

## Overview

* **eorm** - base module for declate entities
* **eorm_object** - helper module for a work with objects
* **eorm_db** - module for a work with db

For details see:

* [Definition](docs/definition.md)
* [Queries](docs/queries.md)


## Other analogues

* [BossDB](https://github.com/ErlyORM/boss_db) - A sharded, caching, pooling, evented ORM for Erlang
* [mekao](https://github.com/ddosia/mekao) - Erlang SQL constructor
* [Sqerl](https://github.com/devinus/sqerl) - An Erlang-flavoured SQL DSL
