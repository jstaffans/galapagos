# galapagos

An implementation of the GraphQL specification in Clojure.

[![Build Status](https://travis-ci.org/jstaffans/galapagos.svg?branch=master)](https://travis-ci.org/jstaffans/galapagos)

## Overview

The goal of this library is to expose the data of your Clojure application in a way that enables
GraphQL clients to interact with it. Special care is taken to make data access as efficient as possible - 
streamlined, concurrent data fetching is provided by the [muse](https://github.com/kachayev/muse) library.

See [links](https://github.com/jstaffans/galapagos/blob/master/links.md) for more information on GraphQL and the ideas behind it.

## Status

Galapagos is still very much pre-alpha software and nearly everything is subject to change.
The first iteration suffers from two problems:

* The schema is defined using a rather complicated, macro-based DSL. There's a lot of metadata being attached to the symbols that make up the various type definitions, which makes schema introspection overly complex. It would be better to just use straight-up Clojure data structures.
* Data fetching has to happen using core.async. This is a design choice that a library like this should not make.

### Queries

Directives and query variables are missing. Validation is lacking.
Support for muse's query batching (a solution to the [1+N query problem] [1]) is not yet implemented.

### Mutation

Mutation operations are not currently supported.

## License

Distributed under the MIT License - see LICENSE for the full license.

[1]: https://github.com/kachayev/muse/blob/master/docs/sql.md
