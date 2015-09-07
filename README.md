# galapagos

An efficient implementation of the GraphQL specification in Clojure.

[![Build Status](https://travis-ci.org/jstaffans/galapagos.svg?branch=master)](https://travis-ci.org/jstaffans/galapagos)

## Overview

The goal of this library is to expose the data of your Clojure application in a way that enables
GraphQL clients to interact with it. Special care is taken to make data access as efficient as possible - 
streamlined, concurrent data fetching is provided by the [muse](https://github.com/kachayev/muse) library.

## Status

Galapagos is still very much pre-alpha software and nearly everything is subject to change.

### Schema definition

The schema definition DSL is still a work-in-progress. Prismatic's schema library is used
for validation and coercion and I'd like to move galapagos' DSL closer to that used in Prismatic/schema. 

Not all GraphQL built-in types are suppored yet. 

### Queries

Directives and query variables are missing. Validation is lacking. Introspection is not yet implemented.
Support for muse's query batching is not yet implemented.

### Mutation

Mutation operations are not currently supported.

## License

Distributed under the MIT License - see LICENSE for the full license.

