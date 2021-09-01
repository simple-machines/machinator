TODO
====

- Dependency computation seems to be fragile.
    - When two modules contain a declaration with the same name, the latter will
      depend on the former. No dependency should be added.
    - When declarations exist with the same name, sometimes a dependency will be
      added with the module name concatenated with itself.

- Python classes with no fields have too much white space.

- Python variants need de-/serialisation functionality in the base class.

- Python docstring generation is a bit flaky.

- Dependency information should be used to generate precise imports.
