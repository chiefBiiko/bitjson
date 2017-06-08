# bitjson

De/serialization utility based on JSON bit arrays.

## Features

The de/serialization functions that form the core of this package allow
un/marshalling *any* R object from/to a JSON bit array while providing:

+ perfect data consistency
+ perfectly valid JSON
+ compression (simple run-length encoding)

## Get it

```r
devtools::install_github('chiefBiiko/bitjson')
```