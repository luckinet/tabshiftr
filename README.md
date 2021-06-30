
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tabshiftr <a href='https://ehrmanns.github.io/tabshiftr/'><img src='man/figures/logo.svg' align="right" height="200" /></a>

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/tabshiftr)](https://cran.r-project.org/package=tabshiftr)
[![Travis-CI Build
Status](https://travis-ci.org/EhrmannS/tabshiftr.svg?branch=master)](https://travis-ci.org/EhrmannS/tabshiftr)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/EhrmannS/tabshiftr?branch=master&svg=true)](https://ci.appveyor.com/project/EhrmannS/tabshiftr)
[![Coverage
Status](https://img.shields.io/codecov/c/github/EhrmannS/tabshiftr/master.svg)](https://codecov.io/github/EhrmannS/tabshiftr?branch=master)
[![](http://cranlogs.r-pkg.org/badges/grand-total/tabshiftr)](https://cran.r-project.org/package=tabshiftr)
[![Lifecycle:maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html)

## Overview

Data are stored in many different ways in tables or spreadsheets because
no strict semantic or topographic standards for the organisation of
tables are commonly accepted. In the R environment the *tidy* paradigm
is a first step towards interoperability of data, in that it requires a
certain arrangement of tables, where variables are recorded in columns
and observations in rows (see <https://tidyr.tidyverse.org/>). Tables
can be tidied (i.e., brought into a tidy arrangement) via packages such
as `tidyr`, however, all functions that deal with reshaping tables to
date require data that are already organised into topologically
coherent, rectangular tables. This is often violated in practice,
especially in data that are scraped off of the internet.

`tabshiftr` fills this gap in the toolchain towards more interoperable
data via `schema` descriptions and a `reorganise()` function that is
largely based on `tidyr`.

## Installation

1)  Install the official version from CRAN:

<!-- end list -->

``` r
install.packages("tabshiftr")
```

or the latest development version from github:

``` r
devtools::install_github("EhrmannS/tabshiftr")
```

2)  The
    [vignette](https://ehrmanns.github.io/tabshiftr/articles/tabshiftr.html)
    gives an introduction, provides an instruction on how to set up
    schema descriptions by going step by step through certain dimensions
    of disorganisation to show which table arrangements can be
    reorganised and how that works.

## Examples

A disorganised table may look like the following table:

``` r
library(tabshiftr)
library(knitr)

# a rather disorganised table with messy clusters and a distinct variable
input <- tabs2shift$clusters_messy
kable(input)
```

| X1          | X2        | X3         | X4          | X5        | X6         | X7     |
| :---------- | :-------- | :--------- | :---------- | :-------- | :--------- | :----- |
| commodities | harvested | production | .           | .         | .          | .      |
| unit 1      | .         | .          | .           | .         | .          | .      |
| soybean     | 1111      | 1112       | year 1      | .         | .          | .      |
| maize       | 1121      | 1122       | year 1      | .         | .          | .      |
| soybean     | 1211      | 1212       | year 2      | .         | .          | .      |
| maize       | 1221      | 1222       | year 2      | .         | .          | .      |
| .           | .         | .          | .           | .         | .          | .      |
| commodities | harvested | production | commodities | harvested | production | .      |
| unit 2      | .         | .          | unit 3      | .         | .          | .      |
| soybean     | 2111      | 2112       | soybean     | 3111      | 3112       | year 1 |
| maize       | 2121      | 2122       | maize       | 3121      | 3122       | year 1 |
| soybean     | 2211      | 2212       | soybean     | 3211      | 3212       | year 2 |
| maize       | 2221      | 2222       | maize       | 3221      | 3222       | year 2 |

If we were to transform this data into tidy data by merely using the
functions in `tidyr` (or the extended `tidyverse` in general), we’d
potentially end up with a massive algorithm, especially for such
complicated table arrangements. For other tables that may or may not be
as complicated, we’d have to set up yet more algorithms and while a
pipeline of tidy functions is relatively easy to set up, it would still
become very laborious to repeat this for the dozens of potential table
arrangements. In `tabshiftr` we solve that by describing the schema of
the input table and providing this schema description to the
`reorganise()` function. This requires us to use a vastly smaller set of
code and makes it thus a lot more efficient to bring multiple
heterogeneous data into an interoperable format.

``` r
# put together schema description by ...
# ... identifying cluster positions
schema <- setCluster(id = "territories", left = c(1, 1, 4), top = c(1, 8, 8))

# ... specifying the cluster ID as id variable (obligatory for when we deal with clusters)
schema <- schema %>%
   setIDVar(name = "territories", columns = c(1, 1, 4), rows = c(2, 9, 9))

# ... specifying a distinct variable (explicit position)
schema <- schema %>%
   setIDVar(name = "year", columns = 4, rows = c(3:6), distinct = TRUE)

# ... specifying a tidy variable (by giving the column values)
schema <- schema %>%
   setIDVar(name = "commodities", columns = c(1, 1, 4))

# ... identifying the (tidy) observed variables
schema <- schema %>%
   setObsVar(name = "harvested", columns = c(2, 2, 5)) %>%
   setObsVar(name = "production", columns = c(3, 3, 6))

# to potentially debug the schema description, first validate the schema ...
schema_valid <- validateSchema(schema = schema, input = input)

# ... and extract parts of it
getIDVars(schema = schema_valid, input = input)
#> [[1]]
#> [[1]]$year
#> # A tibble: 4 x 1
#>   X4    
#>   <chr> 
#> 1 year 1
#> 2 year 1
#> 3 year 2
#> 4 year 2
#> 
#> [[1]]$commodities
#> # A tibble: 4 x 1
#>   X1     
#>   <chr>  
#> 1 soybean
#> 2 maize  
#> 3 soybean
#> 4 maize  
#> 
#> 
#> [[2]]
#> [[2]]$year
#> # A tibble: 4 x 1
#>   X4    
#>   <chr> 
#> 1 year 1
#> 2 year 1
#> 3 year 2
#> 4 year 2
#> 
#> [[2]]$commodities
#> # A tibble: 4 x 1
#>   X1     
#>   <chr>  
#> 1 soybean
#> 2 maize  
#> 3 soybean
#> 4 maize  
#> 
#> 
#> [[3]]
#> [[3]]$year
#> # A tibble: 4 x 1
#>   X4    
#>   <chr> 
#> 1 year 1
#> 2 year 1
#> 3 year 2
#> 4 year 2
#> 
#> [[3]]$commodities
#> # A tibble: 4 x 1
#>   X4     
#>   <chr>  
#> 1 soybean
#> 2 maize  
#> 3 soybean
#> 4 maize
getObsVars(schema = schema_valid, input = input)
#> [[1]]
#> [[1]]$harvested
#> # A tibble: 4 x 1
#>   X2   
#>   <chr>
#> 1 1111 
#> 2 1121 
#> 3 1211 
#> 4 1221 
#> 
#> [[1]]$production
#> # A tibble: 4 x 1
#>   X3   
#>   <chr>
#> 1 1112 
#> 2 1122 
#> 3 1212 
#> 4 1222 
#> 
#> 
#> [[2]]
#> [[2]]$harvested
#> # A tibble: 4 x 1
#>   X2   
#>   <chr>
#> 1 2111 
#> 2 2121 
#> 3 2211 
#> 4 2221 
#> 
#> [[2]]$production
#> # A tibble: 4 x 1
#>   X3   
#>   <chr>
#> 1 2112 
#> 2 2122 
#> 3 2212 
#> 4 2222 
#> 
#> 
#> [[3]]
#> [[3]]$harvested
#> # A tibble: 4 x 1
#>   X5   
#>   <chr>
#> 1 3111 
#> 2 3121 
#> 3 3211 
#> 4 3221 
#> 
#> [[3]]$production
#> # A tibble: 4 x 1
#>   X6   
#>   <chr>
#> 1 3112 
#> 2 3122 
#> 3 3212 
#> 4 3222

# alternatively, relative values starting from the cluster origin could be set
schema_alt <- setCluster(id = "territories",
                         left = c(1, 1, 4), top = c(1, 8, 8)) %>%
  setIDVar(name = "territories", columns = 1, rows = 2, relative = TRUE) %>%
  setIDVar(name = "year", columns = 4, rows = c(3:6), distinct = TRUE) %>%
  setIDVar(name = "commodities", columns = 1, relative = TRUE) %>%
  setObsVar(name = "harvested", columns = 2, relative = TRUE) %>%
  setObsVar(name = "production", columns = 3, relative = TRUE)
```

The `reorganise()` function carries out the steps of validating,
extracting the variables, pivoting the tentative output and putting the
final table together automatically, so it merely requires the finalised
`schema` and the `input` table.

``` r
schema # has a pretty print function
#>   3 clusters
#>     origin : 1|1, 8|1, 8|4  (row|col)
#>     id     : territories
#> 
#>    variable      type       row    col    dist 
#>   ------------- ---------- ------ ------ ------  
#>    territories   id         2, 9   1, 4   F  
#>    year          id         3:6    4      T  
#>    commodities   id                1, 4   F  
#>    harvested     observed          2, 5   F  
#>    production    observed          3, 6   F

output <- reorganise(input = input, schema = schema)
kable(output)
```

| territories | year   | commodities | harvested | production |
| :---------- | :----- | :---------- | --------: | ---------: |
| unit 1      | year 1 | maize       |      1121 |       1122 |
| unit 1      | year 1 | soybean     |      1111 |       1112 |
| unit 1      | year 2 | maize       |      1221 |       1222 |
| unit 1      | year 2 | soybean     |      1211 |       1212 |
| unit 2      | year 1 | maize       |      2121 |       2122 |
| unit 2      | year 1 | soybean     |      2111 |       2112 |
| unit 2      | year 2 | maize       |      2221 |       2222 |
| unit 2      | year 2 | soybean     |      2211 |       2212 |
| unit 3      | year 1 | maize       |      3121 |       3122 |
| unit 3      | year 1 | soybean     |      3111 |       3112 |
| unit 3      | year 2 | maize       |      3221 |       3222 |
| unit 3      | year 2 | soybean     |      3211 |       3212 |

# Contributions

  - tabshiftr is still in development. So far it reliably reorganises 20
    different types of tables, but additional dimensions of
    disorganisation might show themselves. If you encounter a table that
    can’t be reorganised with the current infrastructure, we’d be more
    than happy to collaborate on advancing `tabshiftr`.
  - Informative error management is work in process.
  - Moreover, the resulting schema descriptions can be useful for data
    archiving or database building and `tabshiftr` should at some point
    support that those schemas can be exported into data-formats that
    are used by downstream applications (xml, json, …), following proper
    (ISO) standards. In case you have experience with those standards
    and would like to collab on it, please get in touch\!

# Acknowledgement

This work was supported by funding to Carsten Meyer through the Flexpool
mechanism of the German Centre for Integrative Biodiversity Research
(Div) (FZT-118, DFG).
