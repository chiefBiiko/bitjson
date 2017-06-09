# bitjson

[![Build Status](https://travis-ci.org/chiefBiiko/bitjson.svg?branch=master)](https://travis-ci.org/chiefBiiko/bitjson) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/chiefBiiko/bitjson?branch=master&svg=true)](https://ci.appveyor.com/project/chiefBiiko/bitjson)

De/serialization utility based on JSON bit arrays.

## Features

The de/serialization functions that form the core of this package allow
un/marshalling *any* R object from/to a JSON bit array while providing:

+ perfect data consistency
+ perfectly valid JSON
+ lossless compression (simple run-length encoding)

## Get it

```r
devtools::install_github('chiefBiiko/bitjson')
```