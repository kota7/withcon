<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/kota7/withcon.svg?branch=master)](https://travis-ci.org/kota7/withcon) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/kota7/withcon?branch=master&svg=true)](https://ci.appveyor.com/project/kota7/withcon)

withcon
=======

`withcon` is an R package that provides functionality similar to the `with` clause in Python; It automatically closes connections after a series of operations and also when an exception occurs during the operations.

Currently the functionality is provided for `connection` objects (created *e.g.* by `file` and `gzfile`) and `DBIConnection` objects (created by `DBI::dbConnect`).

Installation
------------

Install from GitHub using the `devtools` library:

``` r
devtools::install_github('kota7/withcon')
```

Example
-------

``` r
library(withcon)
library(DBI)
res <- withCon(conn = dbConnect(RSQLite::SQLite(), ':memory:'), do = {
  dbWriteTable(conn, 'tbl', mtcars)
  dbReadTable(conn, 'tbl')
})
head(res)
#>    mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> 1 21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
#> 2 21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
#> 3 22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
#> 4 21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
#> 5 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
#> 6 18.1   6  225 105 2.76 3.460 20.22  1  0    3    1
```
