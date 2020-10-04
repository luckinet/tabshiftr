
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
[![Lifecycle:maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)

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
coherent, rectangular tables. This is often violated in practice, for
example when data are scraped off of the internet.

`tabshiftr` fills this gap in the toolchain for reproducible data
management via `schema` descriptions and a `reorganise()` function that
is largely based on `tidyr`.

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
library(readr)
library(knitr)

input <- read_csv(file = paste0(system.file("test_datasets", package = "tabshiftr"), "/table_mismatch_3.csv"),
                  col_names = FALSE, col_types = cols(.default = "c"))
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

A schema description can be started with any of the `set*` functions in
this package and for the example table, we start the schema description
by setting the clusters. This table contains two chunks of data at the
rows 1 to 6 and 8 to 13 with a height of 6 cells and they are further
organised into three similar groups (so-called *clusters*) with a width
of 3 cells. In the second row of each cluster is another variable that
is unique for each cluster and thus presumable the cluster identifier,
we call it `territories`.

``` r
library(magrittr)
library(tabshiftr)
schema <- setCluster(id = "territories", top = c(1, 8, 8), left = c(1, 1, 4), 
                     width = 3, height = 6)
```

Next, we recognise that each cluster has variable names in the first
row, so we set those as header. We decide to describe the positions in
terms of values that are relative to the cluster positions, because that
allows us to end up with a more concise schema description.

``` r
schema <- schema %>% 
  setHeader(rows = 1, relative = TRUE)
```

Each cluster contains the identifying variable `commodities` in the
first column and the two observed variables `harvested` and `production`
in the second and third column respectively. Moreover, the identifying
variable `year` has values but no explicit name and is distinct from
clusters (i.e., it doesn’t appear in each cluster). It thus has to be
described with a position that is not relative to clusters, but to the
spreadsheet. Before describing those variables though, we have to make
sure that the cluster ID is set.

``` r
schema <- schema %>% 
  setIDVar(name = "territories", columns = 1, row = 2, relative = TRUE) %>% 
  setIDVar(name = "year", columns = 4, row = c(3:6), distinct = TRUE) %>% 
  setIDVar(name = "commodities", columns = 1, relative = TRUE) %>% 
  setObsVar(name = "harvested", unit = "ha", columns = 2, relative = TRUE) %>% 
  setObsVar(name = "production", unit = "t", columns = 3, relative = TRUE)
```

Input tables may contain many more data/variables than what we are
interested in, but the schema description contains only those variables
that shall be in the output table. Eventually, we end up with the
following schema description.

``` r
schema
#>   3 clusters
#>     origin: 1|1, 8|1, 8|4  (row|col)
#>     id    : territories
#> 
#>    variable      type       row   col   rel   dist 
#>   ------------- ---------- ----- ----- ----- ------  
#>    territories   id         2     1     T     F  
#>    year          id         3:6   4     F     T  
#>    commodities   id               1     T     F  
#>    harvested     observed         2     T     F  
#>    production    observed         3     T     F
```

Finally, the input table is reorganised simply by calling
`reorganise()`.

``` r
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
    different types of tables, all of which are combinations of the
    dimensions of disorganisation outlined in the vignette. We suspect
    that there are further table arrangements, but they are not clear at
    this stage, issues submitted by users and contributors should be
    helpful.
  - Informative error management is work in process.
  - Moreover, the resulting schema descriptions can be useful for data
    archiving or database building and tabshiftr should at some point
    support that those schemas can be exported into data-formats that
    are used by downstream applications (xml, json, …), following proper
    (ISO) standards.

Contributions to those points and discussions on where tabshiftr should
go are highly welcome\!

# Acknowledgement

This work was supported by funding to Carsten Meyer through the Flexpool
mechanism of the German Centre for Integrative Biodiversity Research
(Div) (FZT-118, DFG).
