
# flowery 0.0.1.9000

* Renamed `iterator` class to `flowery_iterator`.

* Exhausted iterators no longer throw when called again. Instead, they
  return `NULL` (#6).

* Fixed top-level `break` statements in loops (#7).


# flowery 0.0.1

Initial release
