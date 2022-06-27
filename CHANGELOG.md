# Changelog

`wai-rate-limit-postgres` uses [PVP Versioning][1].
The changelog is available [on GitHub][2].

## 0.6.0.0

* Update library to work with wai-rate-limit 0.3.0.0 - needs major bump as types are user visible types are changed.
* Now works work GHC 9.2

## 0.5.0.0

* Remove last remaining debug print statement

## 0.4.0.0

* Fix encoding bug by changing key type to `bytea` (#5)

## 0.3.0.0

* Removed unnecessary debug prints

## 0.2.0.0

* Fix bug in table name reference (#1)

## 0.1.0.0

Initial release

## 0.0.0.0

* Initially created.

[1]: https://pvp.haskell.org
[2]: https://github.com/donatello/wai-rate-limit-postgres/releases
