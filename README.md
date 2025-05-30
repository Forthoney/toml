# TOML for Standard ML (MLton)

## Spec Compliance
This library is currently does **not** fully respect the TOML 1.0.0 specification.
For most "well-behaved" TOML documents, this will likely not be a problem.
In the vast majority of cases, this implementation is more lenient than the specification,
accepting documents which are invalid according to the specification.

The goal is to incrementally resolve these parity issues and reach full 1.0.0 compliance.

### Known Issues 
- Allows for redefining/overwriting table entries in situations disallowed by the spec
- Does not check whether a string or comment is valid UTF-8 
