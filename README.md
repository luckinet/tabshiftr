
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tabshiftr <a href='https://ehrmanns.github.io/tabshiftr/'><img src='man/figures/logo.svg' align="right" height="200" /></a>

<!-- badges: start -->

<!-- badges: end -->

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
is largely based on `tidyr`. We call data that are not topologically
coherent *disorganised (messy) data* and they are characterised not only
by "being messy" (as in opposed to tidy), but also by being arranged so
that not all variables are part of the same "chunk" of data or even that
some variables are only available by implication.

## Installation

1)  Install the latest development version from github:

<!-- end list -->

``` r
devtools::install_github("EhrmannS/tabshiftr")
```

2)  The
    [vignette](https://ehrmanns.github.io/tabshiftr/articles/tabshiftr.html)
    provides an instruction on how to set up schema descriptions and
    shows a wide selection of table arrangements and which schemas apply
    to them.

## Examples

A disorganised table may look like the following table:

``` r
library(readr)
library(tabshiftr)
library(knitr)

input <- read_csv(
  file = paste0(system.file("test_datasets", package = "tabshiftr"), "/table13.csv"),
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

This table contains two so-called *clusters*, topologically consistent
chunks of data at the rows 1 to 6 and 8 to 13. Both clusters are
somewhat systematic in that it there are three variables in common,
however, the second cluster contains a subset of variables twice, the
variables `commodities`, `harvested` and `production`. The variable
`year` has values but no explicit name and is not available for each
cluster, and the variable `territories` is not even stored as a typical
variable, but is implicit and noted in the second row of each cluster.

In `tabshiftr` there is a distinction between *identifying (id)
variables*, variables that describe an observational unit, and *measured
variables*, variables that provide some value for that observational
unit. In the example, the variables `territories`, `year` and
`commodities` are id variables and the variables `harvested` and
`production` are measured variables, they give the harvested area and
the production of the given commodity at a given year in a given
territory.

To reorganise any table with `tabshiftr` we need to set up a
[schema](https://en.wikipedia.org/wiki/Database_schema) description. For
each table there is an input schema and an output schema. The input
schema describes how the table looks like and the output schema
describes how the table shall look like after reorganisation. As we want
to end up with tidy tables, i.e., where variables are in columns and
observations in rows, we have already an output schema, a tidy table.

For the input schema we have to describe where which information in a
table can be found, i.e., in which rows and columns. First, we need to
describe whether the table is organised in several clusters, and if so
where they are. In the example, the first cluster starts at `1|1`, the
second at `1|8` and the second at `4|8` (`row|col`). All clusters are 3
cells wide and 6 cells high. Moreover, we already recognised that each
cluster represents one teritorial unit, so we record the variable
`territories` as cluster ID. When theer are several identical clusters,
it makes sense to describe them in terms of `rel`ative values, i.e., not
starting to count from the topmost leftmost cell, but from the start of
the cluster. A table should have some sort of header, i.e., one or more
rows that describes which information a column contains and we need to
register this header, in the example this is the first row of each
cluster.

Input tables may contain many more data/variables than what we are
interested in, but the schema description contains only those variables
we want to have in the output table. For each variable we need to define
at least the `type` and either a `col`umn, or a column and a `row`. Some
variables are `dist`inct from the cluster definition, because they occur
perhaps only once in the table, or are organised in a non-systematic
way, such as the variable `year` in the example. Some variables are
either absent from the table, or are available implicitly, for example
if several spreadsheets or files contain information for one level of an
identifying variable only, for example per commodity. This is not the
case in the example, but if it were so, this variable could be specified
by providing the `value` of the level in the variable. For measured
variables als the target unit and a transformation factor to that unit
can be provided, even though they don’t have any functional meaning.
This results for the `input` table in the following schema description

``` r
library(tabshiftr)
schema <- makeSchema(schema = list(
  clusters =
    list(row = c(1, 8, 8), col = c(1, 1, 4), width = 3, height = 6, id = "territories"),
  header = list(row = 1, rel = TRUE),
  variables =
    list(territories =
           list(type = "id", row = 1, col = 1, rel = TRUE),
         year =
           list(type = "id", row = c(3:6), col = 4, dist = TRUE),
         commodities =
           list(type = "id", col = 1, rel = TRUE),
         harvested =
           list(type = "measured", unit = "ha", factor = 1, col = 2, rel = TRUE),
         production =
           list(type = "measured", unit = "t", factor = 1, col = 3, rel = TRUE))
))
```

Some further fields that have not been mentioned here are:

  - `split`, to seperate an identifying variable via a regular
    expression from some column
  - `key` and `value`, when the names of measured variables are
    aggregated into a column while the values are in a seperate column.
    Here, `key` is the column that contains the names of the measured
    variables and `value` is whatever the measured variable is called
    that should be "pulled" out of `key`.

<!-- end list -->

``` r
schema
#>   3 clusters
#>     origin: 1|1, 8|1, 8|4  (row|col)
#>     id    : territories
#> 
#>    variable      type       row   col   rel   dist 
#>   ------------- ---------- ----- ----- ----- ------  
#>    territories   id         1     1     T     F  
#>    year          id         3:6   4     F     T  
#>    commodities   id               1     T     F  
#>    harvested     measured         2     T     F  
#>    production    measured         3     T     F
```

Finally, the input table is reorganised simply by calling
`reorganise()`.

``` r
output <- reorganise(input = input, schema = schema)
output
#> # A tibble: 12 x 5
#>    territories year   commodities harvested production
#>    <chr>       <chr>  <chr>           <dbl>      <dbl>
#>  1 unit 1      year 1 maize            1121       1122
#>  2 unit 1      year 1 soybean          1111       1112
#>  3 unit 1      year 2 maize            1221       1222
#>  4 unit 1      year 2 soybean          1211       1212
#>  5 unit 2      year 1 maize            2121       2122
#>  6 unit 2      year 1 soybean          2111       2112
#>  7 unit 2      year 2 maize            2221       2222
#>  8 unit 2      year 2 soybean          2211       2212
#>  9 unit 3      year 1 maize            3121       3122
#> 10 unit 3      year 1 soybean          3111       3112
#> 11 unit 3      year 2 maize            3221       3222
#> 12 unit 3      year 2 soybean          3211       3212
```
