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
