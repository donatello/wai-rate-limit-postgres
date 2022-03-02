# wai-rate-limit-postgres

[![GitHub CI](https://github.com/donatello/wai-rate-limit-postgres/workflows/CI/badge.svg)](https://github.com/donatello/wai-rate-limit-postgres/actions)
[![Hackage](https://img.shields.io/hackage/v/wai-rate-limit-postgres.svg?logo=haskell)](https://hackage.haskell.org/package/wai-rate-limit-postgres)
[![Apache-2.0 license](https://img.shields.io/badge/license-Apache--2.0-blue.svg)](LICENSE)

This is a companion package to [wai-rate-limit](https://github.com/mbg/wai-rate-limit) that adds support to use PostgreSQL as a backend.

Depending on traffic and latency of PostgreSQL, this backend may or may not be appropriate for you.

# Testing locally with Docker

Start a PostgreSQL docker container in a terminal:


```shell

$ docker run --name some-postgres -e POSTGRES_PASSWORD=postgres -p 5432:5432 -it --rm postgres -c log_statement=all

```

Run tests in another terminal with:

```shell

$ export PG_DB_URI=postgres://postgres:postgres@localhost:5432/postgres
$ cabal test
```
