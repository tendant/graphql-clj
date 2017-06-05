# Change Log
All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

## [Unreleased][unreleased]

## [0.2.4] -2017-06-05
- Add support comment as description
- Add support for operation name
- Rewrite execution engine for error handling
- Add test cases for execution engine
- Improve performance for error location data

## [0.2.3] - 2017-05-18
- High performance JAVA parser
- Parser will throw Exception with errors in ex-data

## [0.2.2] - 2017-04-27
- Add support for list-type argument #53

## [0.2.1] - 2017-04-27
- Fix query validator for argument with list and InputObject

## [0.2.0] - 2017-04-23
- Enhance parser to support error reporting on lineno, column and index
- Rewrite validator
- [BREAKING] Separate parser function to graphql-cll.parser/parse-schema and graphql-clj.parser/parse-query-document
- [BREAKING] Add schema validator: graphql-clj.schema-validator/validate-schema
- [BREAKING] Rewrite query validator: graphql-clj.query-validator/validate-query

## [0.1.20] - 2016-11-19
- Build arguments, expand fragments, and inline types prior to execution phase
- Optionally add a `:resolver` key to state when validating a statement to inline resolver-fns prior to execution
- Refactor executor to be simpler
- Add a spec for statement validation output and ensure test cases conform
- Warnings for library users accidentally doing parsing / validation in the inner loop
- Streamline statement validation output - remove :state key in the absence of errors
- Bugfix - break on undefined variables, don't traverse deeper
- Bugfix - add test for missing required variables, and support default values for variables
- Bugfix - guard against nil values for required types in the execution phase
- Bugfix - support execution with inline fragments
- Bugfix for introspection resolution - isDeprecated is a non-null field
- Bugfix - validation for missing root fields
- Bugfix - required arguments fulfilled by required variables
- Test - Add case for execution on field arguments with variable bindings
- Test - Add preparation and memoization as an example in executor test
- Add starwars test case from the `graphql-clj-starter project`

## [0.1.19] - 2016-11-16
- Support for Input Objects and their fields in Introspection Schema. By Boris Jonica (bjonica)
- Perform schema and statement validation prior to execution, with more robust error handling (backwards compatible)
- Line and column numbers in validation error output
- Fix bug that could cause an infinite loop in the absence of a parent spec
- Enable memoization of schema and statement preparation prior to execution
- Post-process validated schema to eliminate unnecessary complexity
- Fix potential collision of specs for root query and mutation fields sharing the same type
- Support validation of arguments for root fields
- Fix bug in validation of unused fragments
- Fix bugs in validation of scalar leafs for lists of scalars and lists of objects
- Deprecate `graphql-clj.type/create-schema` in favor of `graphql-clj.validator/validate-schema`
- Fix bug in fragment cycles validation
- Properly assign specs for recursively nested fields (switch to pre-order traversal for adding statement specs)

## [0.1.18] - 2016-10-20
- Validate that statements do not contain fragment cycles
- Validate that fragments are on composite types
- Make validation robust to order (can now reference types declared later in a statement or schema)
- Validate unique variable, operation, input type, fragment, and argument names
- Validate provided non-null arguments
- Validate unused variables and fragments
- Validate known directives
- Validate lone anonymous operation
- Validate variables in allowed positions (works for direct union and interface references, not for nested)
- Dynamically process validation CATs.  Extend validate function to take explicit visitor functions for testing.
- Fix bug in list type specs
- Validate scalar leafs
- Make compatible with spec backport for users of Clojure < 1.9.0

## [0.1.17] - 2016-10-14
- Implement preliminary version of FieldsOnCorrectType validation
- Build a map from namespaced spec keyword to their corresponding nodes
- Include full parent node in each child node (for getting to parent types)
- Validate argument, type, and fragment names
- Validate that variables are input types
- Fix required field argument issue

## [0.1.16] - 2016-10-12
### BREAKING
- [BREAKING] Change resolver to 3-parameter function

### CHANGED
- Pass existing validation CATs (including error messages, mostly, code is still overly complex), including valid and invalid examples
- Namespace schema (and statement variable) types for global spec naming
- Define specs at the end after walking the entire tree (rather than as we go). Support validation of recursive object types.
- Support whitespace at the beginning / end of a schema or statement
- Extra eval protection for defining specs only
- Simplify approach to optional vs. required
- Support types for nested lists, including required
- Split schema and statement validation into 2 passes each - one for resolving specs, two for validation rules using them
- Add a defnodevisitor macro to operate exclusively on nodes of a certain type
- Fix some invalid schema scenarios
- Support required for scalars (but not yet for other types)

## [0.1.15] - 2016-10-11
- Fix aliasing. By Marcin Kulik (sickill).
- Add test for default argument value. By Marcin Kulik (sickill).

## [0.1.14] - 2016-10-10
- Enhance introspection query for arguments and __type
- Only enforce required field arguments
- Support argument default value (#11)
- Handle introspection required type

## [0.1.13] - 2016-10-10
- Return errors under :errors key. By Marcin Kulik (sickill)
- Rename ambiguous `:name` to `:type-name`, `:variable-name`, and `:argument-name` in the appropriate context
- Avoid ambiguity between `BooleanValue` and `EnumValue`
- Reduce code duplication and increase consistency after the parse transform step
- Modify grammar to simplify parser
- Move to a consistent parse tree tracking the type of the node.  Use a vector of maps instead of a single map with multiple key value pairs.
- Fix `:ListType` parsing
- Remove `:enum-value` wrapping key for enum values passed as arguments
- Remove `{:selection { :field }}` wrappers for `:selection-set`
- Preliminary implementation of validation
- Fix introspection query
- Changed from Clojure 1.8.0 to Clojure 1.9.0-alpha12

## [0.1.12] - 2016-09-27
###Changed
- Make `TypeFields`/`InputTypeFields` one to many with `TypeField`/`InputTypeField`
- `TypeFieldArgument` now supports enums
- Resolve name collision between `EnumValue` and `EnumIntValue`
- Change the parsed data structure to a key-value map for `TypeFields`, `InputTypeFields`, and `VariableDefinitions`
- Update parser for variable name
- Unwrap `:type-field-type` for list type for consistency
- Rename ambiguous `:name` to `:type-name` in the context of named types

## [0.1.11] - 2016-09-26
### Changed
- Fix test cases

## [0.1.10] - 2016-09-26
### Added
- Introspection

## [0.1.9] - 2016-09-25
### Changed
- Extract schema to file, support multiple variable definitions separated by commas. By Edward Wible (aew)
- Refactor parser for concision. By Edward Wible (aew)
- Rename render, formatting. By Edward Wible (aew)
- Fix bug in required type field type. By Edward Wible (aew)
- Simplify parsed representation for argument to be keys and values in a map. By Edward Wible (aew)
- Rename :innerType to :inner-type for consistency. By Edward Wible (aew)
- Extract test-helpers for CAT tests. By Edward Wible (aew)
- Add CAT validation scenarios. By Edward Wible (aew)

## [0.1.8] - 2016-09-24
### Added
- Extend parser to pass parser CAT. By Edward Wible (aew)
- Support list of scalars on execution. By Edward Wible (aew)
- Add support for variable and argument validation

### Changed
- Recur on inner type. By Edward Wible (aew)

## [0.1.7] - 2016-09-18

## 0.1.6 - 2016-09-06

## 0.1.5 - 2016-08-28

[unreleased]: https://github.com/tendant/graphql-clj/compare/0.2.4...HEAD
[0.2.4]: https://github.com/tendant/graphql-clj/compare/0.2.3...0.2.4
[0.2.3]: https://github.com/tendant/graphql-clj/compare/0.2.2...0.2.3
[0.2.2]: https://github.com/tendant/graphql-clj/compare/0.2.1...0.2.2
[0.2.1]: https://github.com/tendant/graphql-clj/compare/0.2.0...0.2.1
[0.2.0]: https://github.com/tendant/graphql-clj/compare/0.1.20...0.2.0
[0.1.20]: https://github.com/tendant/graphql-clj/compare/0.1.19...0.1.20
[0.1.19]: https://github.com/tendant/graphql-clj/compare/0.1.18...0.1.19
[0.1.18]: https://github.com/tendant/graphql-clj/compare/0.1.17...0.1.18
[0.1.17]: https://github.com/tendant/graphql-clj/compare/0.1.16...0.1.17
[0.1.16]: https://github.com/tendant/graphql-clj/compare/0.1.15...0.1.16
[0.1.15]: https://github.com/tendant/graphql-clj/compare/0.1.14...0.1.15
[0.1.14]: https://github.com/tendant/graphql-clj/compare/0.1.13...0.1.14
[0.1.13]: https://github.com/tendant/graphql-clj/compare/0.1.12...0.1.13
[0.1.12]: https://github.com/tendant/graphql-clj/compare/0.1.11...0.1.12
[0.1.11]: https://github.com/tendant/graphql-clj/compare/0.1.10...0.1.11
[0.1.10]: https://github.com/tendant/graphql-clj/compare/0.1.9...0.1.10
[0.1.9]: https://github.com/tendant/graphql-clj/compare/0.1.8...0.1.9
[0.1.8]: https://github.com/tendant/graphql-clj/compare/0.1.7...0.1.8
[0.1.7]: https://github.com/tendant/graphql-clj/compare/0.1.6...0.1.7
