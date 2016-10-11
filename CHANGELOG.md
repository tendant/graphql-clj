# Change Log
All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

## [Unreleased][unreleased]

## [0.1.13] - 2016-10-10
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

[unreleased]: https://github.com/tendant/graphql-clj/compare/0.1.13...HEAD
[0.1.13]: https://github.com/tendant/graphql-clj/compare/0.1.12...0.1.13
[0.1.12]: https://github.com/tendant/graphql-clj/compare/0.1.11...0.1.12
[0.1.11]: https://github.com/tendant/graphql-clj/compare/0.1.10...0.1.11
[0.1.10]: https://github.com/tendant/graphql-clj/compare/0.1.9...0.1.10
[0.1.9]: https://github.com/tendant/graphql-clj/compare/0.1.8...0.1.9
[0.1.8]: https://github.com/tendant/graphql-clj/compare/0.1.7...0.1.8
[0.1.7]: https://github.com/tendant/graphql-clj/compare/0.1.6...0.1.7
