## Project Status

This library is in the early stage. It is still under **active** development. All functions are subject to change. Please file an issue or submit a PR if you have suggestions!

The implementation of the library follow closely to the GraphQL Draft RFC Specification (https://facebook.github.io/graphql/).

- [x] GraphQL parser
    * [x] Query
    * [x] Mutation
    * [x] Type System
    * [x] Variables
- [x] Transformation
- [x] Execution
    * [x] Query
    * [x] Mutation
    * [x] Support List Type
    * [x] Support Non-Null Type
    * [x] Arguments
    * [x] Variables
        * [x] List type variable
    * [ ] Union
    * [ ] Interface
    * [ ] Safe parallel execution
    * [ ] Coerce
- [x] Fragment execution
    * [ ] Fragment Type
- [x] Support Context
- [x] Type Introspect
    * [x] Type introspection schema (http://graphql.org/docs/introspection/)
    * [x] Type introspection query (http://facebook.github.io/graphql/#sec-Introspection)
    * [x] Arguments
    * [ ] Input Variables
    * [ ] Interfaces
- [ ] Directives
- [x] Testing
    * [ ] Use clojure.spec to generate test resolver automatically
- [X] Schema validation
- [X] Query validation
    * [ ] Overlapping fields can be merged (https://github.com/graphql/graphql-js/blob/master/src/validation/rules/OverlappingFieldsCanBeMerged.js)
- [X] Arguments validation
    * [ ] Argument Coerce
- [X] Variables validation
    * [ ] Variable Coerce
- [X] Parser error handling
- [ ] Execution error handling
- [ ] Batch data loading
- [ ] Comment as meta data
    * https://github.com/facebook/graphql/issues/200
    * https://github.com/graphql-java/graphql-java/issues/183
- [ ] Relay support
    * https://facebook.github.io/relay/docs/graphql-relay-specification.html
    * https://facebook.github.io/relay/graphql/connections.htm
    * https://facebook.github.io/relay/graphql/objectidentification.htm
    * https://facebook.github.io/relay/graphql/mutations.htm

