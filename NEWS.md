# tabshiftr 0.4.0 - grouping update

- introduction of the function `setGroups()`, which allows to summarize columns and/or rows according to a function. The positions and summarise function here are provided via the new function `.sum()`.
- now each getter ensures that it only gets things from an already validated schema.
- for filter and group slots of a schema there is now a new way implemented for storing locations. Each location statement is now a list that contains either `find`, `group` or `position` parameters.

# tabshiftr 0.3.9

- allow several filters to be applied in a separate function and thus for separate columns.
- allow filtering on columns

# tabshiftr 0.3.8

- allow `.find`ing based on several character strings (that can themselves be regular expressions).

# tabshiftr 0.3.7

- implement code so that duplicated observations in the input table are by default summarized (with a warning)

# tabshiftr 0.3.6

- fix a bug that would not allow to register of two wide identifying variables first the wider, then the narrower wide identifying variable.

# tabshiftr 0.3.5

- fix a bug that would not allow tables that have only one row and implicit variables to be properly reorganized.

# tabshiftr 0.3.4

- incl. handling of flags (character additions to the numeric values of observed variables), which are now extracted into an ancillary column.

# tabshiftr 0.3.3

- fix a bug where the header was not properly recognized when using `setFilter()`.

# tabshiftr 0.3.2

- be more precise in using tidyr
- revising some text here and there
- other minor fixes

# tabshiftr 0.3.1

- various minor bugfixes
- minor reorganization of the internal workings

# tabshiftr 0.3.0 - getters update

- introduction of getters that fulfill two functions: 1) check up on the schema one is currently building, 2) internal use to streamline the code-base
- less cryptic naming of the example files that they can be understood without much explanation
- complete revisit of the code-base
- removal of the function `setHeader()`, as these information are now automatically extracted from the functions
- renaming of the argument `row = ` in `setObsVar()` to `top = `, because this is represents a clearer meaning of what this argument describes
- `.find()` now also allows functions that evaluate to TRUE/FALSE
- add a column specifier to `.find()`.


# tabshiftr 0.2.6

- properly enable use of functions in `.find` via several bug-fixes

# tabshiftr 0.2.5

- internal updates/bug-fixes

# tabshiftr 0.2.4

- new function `setFilter` to provide which columns/rows should be ignored by a schema.
- handling of relative positions was changed internally to simplify the code.

# tabshiftr 0.2.3

- generalise `find_col` and `find_row` to `.find` and handle it based on the context it's called in.
- include the option to specify clusters that are members of irregular groups of clusters associated to a variable that shall be recorded itself, `setCluster(..., group = "grouping_variable", member = c(1, 1, 2, ...))`.

# tabshiftr 0.2.2

- include functions `find_col` and `find_row` to determine columns and rows \"on the fly\"

# tabshiftr 0.2.1

- automatically complete id-columns that contain missing values in some rows.
- automatically remove rows and columns that contain merely NA values.

# tabshiftr 0.2.0 - setters update

- include functions `setCluster`, `setHeader`,`setFormat`, `setIDVar` and `setObsVar` for easier schema setup.
- improved documentation
- first steps in implementing errors/warnings when setting up a schema that will likely not result in a successful reorganisation


# tabshiftr 0.1.4

- include a control that user-provided "." as decimal symbol is turned into "[.]" to be used as regular expression.
- re-enable completion of when values in the header are missing. Values are now completed with their left neighbour, as this is the most frequent use-case when values in the header are omitted.

# tabshiftr 0.1.3

- enable the field 'merge = character(1)' for identifying variables, which would be used when a variable is spread over several columns that need to be combined.

# tabshiftr 0.1.2

initial release to CRAN
