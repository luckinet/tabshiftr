
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
is largely based on `tidyr`. We call data that are not topologically
coherent *disorganised (messy) data* and they are characterised not only
by "being messy" (as in opposed to tidy), but also by being arranged so
that not all variables are part of the same "chunk" of data or even that
some variables are only available by implication.

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
    provides an instruction on how to set up schema descriptions and
    shows the table arrangements that can be reorganised in this
    version, and which schemas apply to them.

## Examples

A disorganised table may look like the following table:

``` r
library(readr)
library(tabshiftr)
library(knitr)

input <- read_csv(file = paste0(system.file("test_datasets", package = "tabshiftr"), "/table13.csv"),
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

This table contains two chunks of data at the rows 1 to 6 and 8 to 13
and they are further organised into three similar groups (so-called
*clusters*). The clusters are somewhat systematic in that there are
three variables in common, `commodities`, `harvested` and `production`.
The variable `year` has values but no explicit name and is not available
for each cluster, and the variable `territories` is not even stored in a
column, but is implicit and noted in the second row of each cluster.

In `tabshiftr` we distinguish between *identifying variables*, variables
that describe an observational unit, and *measured variables*, variables
that provide some value for that observational unit. In the example,
`territories`, `year` and `commodities` are identifying variables and
`harvested` and `production` are measured variables, they give the
harvested area and the production of the given commodity at a given year
in a given territory.

The approach of `tabshiftr` is based on capturing the arrangement of
tables in so-called
[schema](https://en.wikipedia.org/wiki/Database_schema) description.
Typically there is an input and an output schema, describing the
arrangement of the input and output tables, respectively. The advantage
of this procedure is that input and output tables exist explicitly and
the schema maps the transformation of the data. As we want to end up
with tidy tables the output schema is pre-determined by a tidy table of
the included variables, but the input schema description needs to be
provided by the user.

For the input schema we have to describe, according to a [hierarchical
set of
rules](https://ehrmanns.github.io/tabshiftr/articles/tabshiftr.html#setting-up-schema-descriptions),
in which rows and columns of a table which information can be found (see
the example below). First, we need to describe whether the table is
organised in several clusters, and if so where they are located. In the
example, the "origin" of the first cluster is at the first row from the
top and the first cell from left (`1|1`), the second at `1|8` and the
third cluster originates at `4|8`. All clusters are 3 cells wide and 6
cells high. Moreover, we already recognised that each cluster represents
one territorial unit, so we record the variable `territories` as cluster
ID. A table should have some sort of header, i.e., one or more rows that
describes which information a column contains and we need to register
this header, in the example this is the first row of each cluster. When
there are several clusters that are identical in arrangement (but not in
values), it makes sense to describe them in terms of `rel`ative values,
i.e., starting to count from the origin of the cluster instead of the
spreadsheet.

Input tables may contain many more data/variables than what we are
interested in, but the schema description contains only those variables
we want to have in the output table. For each variable we need to define
at least the `type` and either `col`umn(s), or column(s) and `row`(s).
Some variables are `dist`inct from the cluster outline, because they
occur perhaps only once in the table, or are organised in a
non-systematic way, such as the variable `year` in the example. Some
variables are either absent from the table, or are available only
implicitly, for example if several spreadsheets or files contain
information for one level of an identifying variable, such as per
commodity. This is not the case in the example, but if it were so, this
variable could be specified by providing the `value` of the level in the
variable. For measured variables we also need to provide the target unit
and a transformation factor to that unit, the latter of which will be
multiplied with the values of the measured variable. This results in the
following schema description for our example:

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

  - `split`, to separate an identifying variable via a regular
    expression from some column
  - `key` and `value`, when the names of measured variables are provided
    in a column while the respective values are in a separate column.
    Here, `key` is the column that contains the names and `value` is the
    level of the measured variable that should be "pulled" out of `key`.

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
kable(output)
```

| territories | year   | commodities | harvested | production |
| :---------: | :----- | :---------- | --------: | ---------: |
|   unit 1    | year 1 | maize       |      1121 |       1122 |
|   unit 1    | year 1 | soybean     |      1111 |       1112 |
|   unit 1    | year 2 | maize       |      1221 |       1222 |
|   unit 1    | year 2 | soybean     |      1211 |       1212 |
|   unit 2    | year 1 | maize       |      2121 |       2122 |
|   unit 2    | year 1 | soybean     |      2111 |       2112 |
|   unit 2    | year 2 | maize       |      2221 |       2222 |
|   unit 2    | year 2 | soybean     |      2211 |       2212 |
|   unit 3    | year 1 | maize       |      3121 |       3122 |
|   unit 3    | year 1 | soybean     |      3111 |       3112 |
|   unit 3    | year 2 | maize       |      3221 |       3222 |
|   unit 3    | year 2 | soybean     |      3211 |       3212 |

# Contributions

  - tabshiftr is still in development. So far it reliably reorganises 19
    different types of tables all of which are combinations of the four
    dimensions of disorganisation outlined in the vignette. We suspect
    that there are further table arrangements, but they are not clear at
    this stage, issues submitted by users and contributors should be
    helpful.
  - Moreover, the resulting schema descriptions can be useful for data
    archiving or database building and tabshiftr should at some point
    support that those schemas can be exported into data-formats that
    are used by downstream applications (xml, json, …), following proper
    (ISO) standards.
  - We envision for a next version some functions for assisted
    construction of the schema. Those could for instance be
    `setCluster()`, `setHeader()` and `setVariable()`, which could be
    piped into one another and finally into `makeSchema()`.
  - A proper error-management. Due to the fuzzy nature of setting up a
    schema description, interaction between machine and human is
    crucial. Whenever an instruction to extract information from a table
    results in an error, it should ideally be possible to pinpoint what
    exactly the issue with those instruction is. This might be a
    challenge, but it should be included in a future version of
    tabshiftr.

Contributions to those points and discussions on where tabshiftr should
go are highly welcome\!

# Acknowledgement

This work was supported by funding to Carsten Meyer through the Flexpool
mechanism of the German Centre for Integrative Biodiversity Research
(Div) (FZT-118, DFG).
