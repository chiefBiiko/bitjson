bitjson-intro
================
2017-06-17

-   [Why](#why)
-   [Examples](#examples)
    -   [Data consistency](#data-consistency)
    -   [IO](#io)
-   [Data format](#data-format)
    -   [Uncompressed bit JSON](#uncompressed-bit-json)
    -   [Compressed bit JSON](#compressed-bit-json)
-   [Compression](#compression)
    -   [Iterative compression algorithm](#iterative-compression-algorithm)
    -   [Iterative decompression algorithm](#iterative-decompression-algorithm)

Why
---

..not? Just wanted to un/marshal `R` objects from/to a text representation that perfectly preserves data consistency, send it over a *wire* or just dump it anywhere.

Examples
--------

### Data consistency

``` r
# marshal to bit JSON
nilebits <- bitjson::toBitJSON(datasets::Nile)

# unmarshal from bit JSON
nile <- bitjson::fromBitJSON(nilebits)

# marshaled still consistent
cat('consistent:', identical(datasets::Nile, nile))
```

    consistent: TRUE

### IO

`bitjson::toBitJSON` allows writing `bitjson` arrays directly to disk by making use of parameter `file`. Since `bitjson` depends on `jsonlite` for conversion between `JSON` arrays and `R` integer vectors it inherits `jsonlite's` powerful IO features. Therefore, `bitjson::fromBitJSON` can unmarshal from a file, url or in-memory `JSON` string.

``` r
# write to disk
bitjson::toBitJSON(datasets::islands, file='islands.json')

# read from disk
inlands <- bitjson::fromBitJSON('islands.json')

# after io roundtrip
cat('consistent via disk:', identical(datasets::islands, inlands))
```

    consistent via disk: TRUE

Data format
-----------

`bitjson` uses numeric `JSON` arrays as underlying data structure. A `bitjson` array contains either zeros and ones exclusively (uncompressed) or a sequence of unsigned integers (compressed). In either case it is valid `JSON`.

`bitjson::toBitJSON` applies compression by default; toggleable via parameter `compress`. Similarly `bitjson::fromBitJSON` expects a compressed bit `JSON` array by default, which likewise can be toggled via parameter `compressed`. Better to use compression though.

### Uncompressed bit JSON

``` r
# just a demo - do use compression 
xl <- bitjson::toBitJSON(419L, compress=FALSE)

# uncompressed xl bit JSON array
cat('uncompressed:\n', xl, sep='')
```

    uncompressed:
    [0,0,0,1,1,0,1,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,1,0,0,0,1,0,1]

### Compressed bit JSON

``` r
# parameter compress defaults to TRUE
xs <- bitjson::toBitJSON(419L)

# compressed bit JSON array
cat('compressed:\n', xs, sep='')
```

    compressed:
    [3,0,2,1,0,1,2,0,1,0,1,29,0,1,14,0,2,1,6,0,2,1,7,0,1,15,0,1,6,0,2,1,38,0,1,0,2,1,28,0,1,23,0,1,7,0,2,1,3,0,1,0,1]

Compression
-----------

Since bit arrays can get rather vast, `bitjson` uses a simple de/compression approach that grounds on [run-length encoding](https://en.wikipedia.org/wiki/Run-length_encoding). A notable property of the applied compression algorithm is zero encoding overhead, meaning the compressed array will in no case be longer than its uncompressed counterpart. To speed things up the de/compression algorithms are implemented in `C++` via `Rcpp`.

### Iterative compression algorithm

-   Setup a return array, `rtn`, that will grow *on the fly*
-   Initialize a count variable, `cnt`, that captures each run's length to `0`
-   Initialize a *lookbehind* variable, `prev`, that holds the bit at `i - 1` each iteration to the first bit in the input array (`i` being the index of the current element in the bit array each iteration)
-   Iterate the input bit array
    -   if the bit at index `i` is not equal to `prev`, record the bit run of `prev` in `rtn`:
        -   if the run-length of `prev` aka `cnt` is equal to `1` append `prev` to `rtn`
        -   otherwise, if the run-length of `prev` aka `cnt` is greater than `1` append `cnt` to `rtn`, then append `prev` to `rtn`
        -   reset `cnt` to `0`
    -   increment `cnt`
    -   assign the bit at index `i` to `prev`
-   Consume remainder/traling bit(run):
    -   if the run-length of `prev` aka `cnt` is equal to `1` append `prev` to `rtn`
    -   otherwise, if the run-length of `prev` aka `cnt` is greater than `1` append `cnt` to `rtn`, then append `prev` to `rtn`
-   Return `rtn`

### Iterative decompression algorithm

-   Setup a return array, `rtn`, that will grow *on the fly*
-   Initialize a *lookbehind* variable, `prev`, that holds the bit at `i - 1` each iteration to `0` (`i` being the index of the current element in the bit array each iteration)
-   Iterate the compressed input (integer) array
    -   if `prev` is greater `1` AND the integer at index `i` is either `0` or `1` append the integer at index `i` to `rtn` `prev` times
    -   otherwise, if the integer at index `i` is either `0` or `1` append the integer at index `i` to `rtn`
    -   assign the integer at index `i` to `prev`
-   Return `rtn`
