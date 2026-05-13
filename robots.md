# tabshiftr - reference for LLMs

This document is the single source of truth for the tabshiftr package. It
exists so an LLM can write correct tabshiftr code from this file alone,
without opening source. When in doubt, this document overrides any
other description. Update this file whenever a function's signature,
behaviour, or the package structure changes.

If you are an LLM reading this: **read the "Common mistakes" section
first**. Most failures with tabshiftr come from a small set of recurring
errors that the section names directly.

---

## Common mistakes

These are the mistakes that recur most often. Read them first.

### 1. tabshiftr describes table layout; it does not parse files.

tabshiftr is **not** a CSV/Excel reader. You read the file yourself
(typically via `read.csv(..., header = FALSE)` or `readxl::read_excel`)
into a data frame, then hand that data frame plus a schema to
`reorganise()`. The schema tells `reorganise` where in the table each
variable sits.

```r
# WRONG - tabshiftr does not read files
schema <- setIDVar(name = "year", file = "data.csv", ...)

# CORRECT - read the file first
input  <- read.csv("data.csv", header = FALSE)
schema <- setIDVar(name = "year", columns = 1)
out    <- reorganise(input = input, schema = schema)
```

### 2. Always read tables with `header = FALSE`.

Row numbers in the schema refer to rows of the in-memory table. If
you read with `header = TRUE`, the header row is consumed into column
names and row 1 of the data is actually row 2 of the file - every
row index in the schema is then off by one and the table breaks when
the header is more than a single row.

```r
# CORRECT - default for read.csv is header = TRUE, so override
input  <- read.csv("data.csv", header = FALSE)
input  <- readxl::read_excel("data.xlsx", col_names = FALSE)

# WRONG - do not do this
input  <- read.csv("data.csv")           # consumes header silently
```

Do not use the `setFormat(header = ...)` argument to compensate. Just
read with `header = FALSE` in the first place.

### 3. The four kinds of variable.

- **Identifying variable** (`setIDVar`): a qualitative property that
  helps identify each observation. Year, region, commodity, etc.
- **Observed variable** (`setObsVar`): a quantitative measurement.
  Harvested area, production, count, etc.
- **Implicit variable**: an identifying variable whose value is the
  same for the whole table (or cluster) and isn't stored in any cell.
  Use `setIDVar(name = "country", value = "Germany")`.
- **Distinct variable**: an identifying variable that lives outside
  the main data block (e.g. in a header cell of each cluster). Use
  `setIDVar(..., distinct = TRUE)`.

### 4. Clusters are topologically coherent sub-tables.

Use `setCluster` when the same set of variables appears more than
once in the table, nested under another variable (e.g. one block per
country, stacked vertically). Pass the top-left cell of each cluster
in `left` and `top` (vectors, one per cluster). Then specify
**variables once** with column/row indices that are vectors of length
== number of clusters.

```r
schema <- setCluster(id = "territories",
                     left = c(1, 1, 4), top = c(1, 8, 8)) |>
          setIDVar(name = "territories",
                   columns = c(1, 1, 4), rows = c(2, 9, 9)) |>
          setIDVar(name = "commodities", columns = c(1, 1, 4)) |>
          setObsVar(name = "harvested", columns = c(2, 2, 5)) |>
          setObsVar(name = "production", columns = c(3, 3, 6))
```

If clusters all share the same layout, set `relative = TRUE` in
`.find()` calls and use single values - they'll be applied
cluster-relative.

### 5. `setCluster(id = "observed")` when the cluster ID is an observed variable.

If clusters are split by an *observed* variable (e.g. one cluster per
commodity, where commodity is otherwise an obs var), pass
`id = "observed"` rather than a cluster-ID variable name. The cluster
position itself encodes the value.

### 6. `setIDVar` column/row meaning is positional.

- `columns = c(...)` is the column(s) where the **values** sit.
- `rows = c(...)` is the row where the **variable name** sits, used
  only when the variable is wide-format (spread across columns).
- `value = "..."` is for implicit variables (no cell in the table
  carries this value).

You set exactly one of `columns` OR `value`. Setting both errors.

### 7. `.find()` is for on-the-fly lookup; not all schemas need it.

If the row/column position of a variable is fixed across all tables
you'll process, hard-code the integer. Only reach for `.find()` when
the position varies between files but the cell content has a
recognisable pattern.

```r
# fixed position
setIDVar(name = "year", columns = 2)

# variable position
setIDVar(name = "year", columns = .find(pattern = "20[0-9]{2}"))
```

`.find()` returns indices computed at `validateSchema` time by
matching the regex or function against the table content.

### 8. `setFilter` and `setGroups` operate on the raw table.

Both apply BEFORE variable extraction. `setFilter(rows = ...)` keeps
only those rows; `setFilter(rows = ..., invert = TRUE)` drops them.
`setGroups(rows = .sum(c(3,4)))` collapses rows 3 and 4 into one row
by summing numerics and pasting characters.

When using `invert = TRUE` to exclude rows, **you must include the
header row** in the exclusion list - it's not auto-preserved.

### 9. `reorganise` returns a tibble; if columns are missing, validate.

If `reorganise()` returns a table missing variables you defined, call
`validateSchema(input, schema)` directly and inspect the result. That
function fills in `.find()` lookups and resolves wildcards; if it
errors, the schema/table mismatch is what to fix.

### 10. Schema export to xml/json/ISO is out of scope.

Schemas are R objects. They can be saved via `saveRDS()` and loaded
later. Do not write or expect serialisation to external formats - that
was an explicit scope decision (2026-05-05).

---

## Object model

### `schema` (S4 class)

A `schema` has three slots:

| Slot | Type | Holds |
|---|---|---|
| `clusters` | named `list` | `id`, `group`, `member`, `left`, `top`, `width`, `height` describing one or more rectangular sub-tables |
| `format` | named `list` | `header`, `decimal`, `thousand`, `na_values`, `zero_values`, `flags` |
| `variables` | named `list` | one entry per declared variable; entries are lists with `type`, `value`, `columns`, `rows`, `top`, `split`, `merge`, `factor`, `key`, `distinct` (subset depending on whether it's an ID or Obs var) |
| `filter` | named `list` | row/column filters added by `setFilter` |
| `groups` | named `list` | row/column aggregations added by `setGroups` |

Schemas are built fluently via `set*()` calls and consumed by
`reorganise()`. Each setter returns a new schema object; chain via
`|>` or `%>%`.

The default empty schema is `schema_default` (internal). Setters
called with `schema = NULL` start from it.

### Lifecycle

```
read input -> set* / .find / .sum -> validateSchema -> validateInput -> reorganise -> tidy output
   (you)        (you, build schema)      (internal)        (internal)       (you)
```

`validateSchema` resolves `.find()` lookups against the input, fills
in implicit positions, and asserts formal consistency. `validateInput`
applies `setGroups` aggregations.

Both validators are exported but called automatically by
`reorganise`. Use them directly only when debugging.

---

## Minimum working example

```r
library(tabshiftr)

# Bundled messy table: territories down rows, year + commodity across
input <- tabs2shift$tidy
input

# Schema: declare which columns are which
schema <- setIDVar(name = "territories", columns = 1)              |>
          setIDVar(name = "year",        columns = 2)              |>
          setIDVar(name = "commodities", columns = 3)              |>
          setObsVar(name = "harvested",  columns = 5)              |>
          setObsVar(name = "production", columns = 6)

# Reorganise into tidy format
reorganise(input = input, schema = schema)
```

For a more involved layout (multi-cluster, distinct variable):

```r
input <- tabs2shift$clusters_messy

schema <- setCluster(id = "territories",
                     left = c(1, 1, 4), top = c(1, 8, 8))          |>
          setIDVar(name = "territories",
                   columns = c(1, 1, 4), rows = c(2, 9, 9))        |>
          setIDVar(name = "year", columns = 4, rows = 3:6,
                   distinct = TRUE)                                |>
          setIDVar(name = "commodities", columns = c(1, 1, 4))     |>
          setObsVar(name = "harvested",  columns = c(2, 2, 5))     |>
          setObsVar(name = "production", columns = c(3, 3, 6))

reorganise(input, schema)
```

---

## set* - declare schema

Each `set*` takes an existing schema (or `NULL` to start a fresh
one) and returns an updated schema. Chain with `|>`.

### `setIDVar(schema, name, type, value, columns, rows, split, merge, distinct)`

Declare an identifying variable.

- `name` variable name in the output (required).
- `type` data type: `"character"` (default), `"integer"`, `"numeric"`,
  `"logical"`, `"Date"`, or `"_"` to skip. Single-letter shortcuts
  (`"c"`, `"i"`, `"n"`, `"l"`, `"D"`) are accepted.
  Dates must be `YYYY-MM-DD`; non-matching values become NA.
- `value` implicit value when the variable isn't in any cell.
- `columns` integer vector, the column(s) where values live.
- `rows` integer vector, the row where variable **names** sit (for
  wide-format variables where the same variable spans multiple
  columns with names in a header row).
- `split` regex to extract this variable out of a compound cell.
  Uses `tidyr::extract` semantics.
- `merge` glue string to join several columns into one variable.
- `distinct = TRUE` if the variable lives outside the main data
  block (e.g. one value per cluster header).

Provide exactly one of `value`, `columns`.

### `setObsVar(schema, name, type, columns, top, factor, key, value, distinct)`

Declare an observed variable.

- `name` variable name (required).
- `type` data type, default `"numeric"`. Same options as `setIDVar`.
- `columns` integer vector.
- `top` row where the variable's **name** sits when nested under a
  wide identifying variable.
- `factor` multiplier applied to raw values (unit conversion).
  Default 1.
- `key` long-format trigger: column index containing variable names
  when several obs vars are stacked in two columns
  (`names_col`, `values_col`). Alternatively `"cluster"` if obs var
  names come from the cluster ID.
- `value` the level in the key column that selects this variable's
  rows.
- `distinct = TRUE` for obs vars recorded outside the main block.

### `setFormat(schema, header, decimal, thousand, na_values, zero_values, flags)`

Declare format quirks of the source table.

- `decimal`, `thousand` single characters.
- `na_values`, `zero_values` character vectors of strings to
  interpret as NA / 0.
- `flags` two-column data frame (`flag`, `value`) of suffixes/markers
  to strip from numeric cells (e.g. `c` for "estimated", `*` for
  "provisional").

### `setCluster(schema, id, group, member, left, top, width, height)`

Declare cluster positions.

- `id` name of the variable that identifies clusters, OR
  `"observed"` if the cluster splits an observed variable.
- `group` name of a higher-level grouping variable when clusters
  are nested.
- `left`, `top` integer vectors with one value per cluster (top-left
  cell coords).
- `width`, `height` cluster size; if NULL, inferred from layout.
- `member` integer vector matching each cluster to its group when
  `group` is set.

### `setFilter(schema, rows, columns, invert, clusters, operator)`

Keep (or drop, with `invert = TRUE`) specific rows or columns before
extraction.

- `rows`/`columns` integer vector or output of `.find()`.
- `invert = TRUE` reverses sense. **When inverting row filters,
  include the header row in `rows` explicitly** - the header isn't
  auto-preserved.
- `clusters = TRUE` (default) applies to cluster rows; FALSE skips.
- `operator` `` `|` `` or `` `&` `` to combine with the preceding
  filter. NULL stacks as AND.

### `setGroups(schema, rows, columns)`

Aggregate groups of rows/columns into single rows/columns before
extraction. Pass the output of `.sum()`:

```r
schema <- setGroups(rows    = .sum(c(3, 4))) |>
          setGroups(columns = .sum(c(5, 6), fill = "down"))
```

---

## .find and .sum - inline helpers

### `.find(fun, pattern, col, row, invert, relative)`

Defer position resolution until `validateSchema` runs. Use either a
regex `pattern` or a function `fun` that returns logical per cell.

- `col`/`row` restrict the search to certain columns or rows
  (otherwise searches everywhere).
- `relative = TRUE` interprets indices relative to a cluster's
  top-left.
- `invert = TRUE` selects the complement.

### `.sum(..., character, numeric, fill)`

Define how a group of rows/columns is reduced to one. Pass the
indices in `...`.

- `character` function to combine character columns; default
  `paste0(na.omit(x), collapse = "-/-")`.
- `numeric` function to combine numeric columns; default
  `sum(x, na.rm = TRUE)`.
- `fill` one of `"down"`, `"up"`, `"right"`, applied **before**
  aggregation to fill NAs.

---

## reorganise

`reorganise(input, schema)`.

The single user-facing entry point that converts a messy input into
a tidy output. Internally:

1. `validateSchema(input, schema)` - resolves `.find()`, fills in
   wildcards, asserts consistency.
2. `validateInput(schema, input)` - applies `setGroups`
   aggregations.
3. Variable extraction loop - reads each declared variable from the
   positions in the schema, applies type coercion and format rules.
4. Assembles the tidy tibble.

Returns a tibble.

---

## validateSchema / validateInput

Both are exported for debugging. Calling them yourself is useful when
`reorganise` produces unexpected output.

- `validateSchema(schema, input)` returns a schema with `.find()`
  calls resolved into concrete integers, missing slots filled, and
  positions sanity-checked.
- `validateInput(schema, input)` returns the input table after
  pre-processing (group aggregation).

---

## getters

Each returns one component of a (validated) schema for inspection.

- `getIDVars(schema)` - list of identifying-variable specs.
- `getObsVars(schema)` - list of observed-variable specs.
- `getClusterVar(schema)` - cluster description.
- `getGroupVar(schema)` - group description.

Primary use: debugging, and as building blocks inside `reorganise`.

---

## schema_builder

`schema_builder(input)`.

A Shiny gadget that opens in the browser (`launch.browser = TRUE`)
and lets the user point-and-click on cells to build a schema
interactively. Returns a `schema` object via `result_env$schema`.

- Click "Finish" to commit the schema and close.
- Closing the browser tab without Finish is treated as cancel - the
  function unblocks and returns `NULL`.

Cluster-ID flow is supported. Phase 1+2 of the builder are complete;
some rarer schema combinations are still being tested against
real-world layouts. If `schema_builder` produces a schema that
`validateSchema` rejects, fall back to writing the schema by hand.

---

## Bundled data: `tabs2shift`

A named list of example messy tables, used in vignettes and tests.
Common entries:

- `tidy` - already-tidy reference table.
- `clusters_messy` - multiple clusters with a distinct variable.
- `messy_rows` - rows that need filtering.
- `group_sum` - rows that need grouping/summing via `setGroups`.
- ... plus 20+ more covering the catalogue of layouts the package
  is designed to handle.

Use `names(tabs2shift)` to list them all.

---

## Conventions

- **Setters are immutable.** Each `set*` returns a new schema. Chain
  via `|>` or `%>%`. Don't try to mutate in place.
- **Indices are 1-based and refer to the in-memory table.** Row 1 is
  the top row of whatever was read into R. Always read with
  `header = FALSE` so row numbers are stable and consistent with the
  schema.
- **`columns` is positional.** Schema indices are integers, not
  column names. Reading with `header = FALSE` keeps them stable.
- **Cluster-aware vectors.** When you have N clusters,
  `columns = c(...)` and `rows = c(...)` must each be length N
  (or length 1 if shared).
- **NA handling is explicit.** Strings to be interpreted as NA must
  be listed in `setFormat(na_values = ...)`. There's no auto-detect.
- **Encoding: UTF-8.** Source declares `Encoding: UTF-8` in
  DESCRIPTION. Non-ASCII in roxygen is allowed; **non-ASCII in R
  source (including comments) breaks CRAN checks**. Use ASCII
  hyphens, not em-dashes.

## Internal vs external boundary

- **Public API** (15 exports): `setIDVar`, `setObsVar`, `setFormat`,
  `setCluster`, `setFilter`, `setGroups`, `.find`, `.sum`,
  `reorganise`, `validateSchema`, `validateInput`, `getIDVars`,
  `getObsVars`, `getClusterVar`, `getGroupVar`, `schema_builder`,
  plus the magrittr pipe re-export.
- **Internal**: `schema_default`, `.sb_*` (schema_builder
  scaffolding), schema validity helpers. Not user-facing.
- **S4 class `schema`**: defined in `R/schema.R`. User code rarely
  constructs one directly; setters are the way.

## Dependencies

`Imports`: checkmate, rlang, tibble, dplyr, tidyr, magrittr,
tidyselect, testthat, crayon, methods, purrr, stringr, lubridate.
`Suggests`: knitr, rmarkdown, bookdown, readr, shiny, DT.

Shiny + DT are suggested because `schema_builder` is optional - if
you write schemas by hand you never need them.
