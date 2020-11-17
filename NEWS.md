# 0.2.1

- automatically complete id-columns that contain missing values in some rows.
- automatically remove rows and columns that contain merely NA values.

# 0.2.0

- include functions `setCluster`, `setHeader`,`setFormat`, `setIDVar` and `setObsVar` for easier schema setup.
- improved documentation
- first steps in implementing errors/warnings when setting up a schema that will likely not result in a successful reorganisation

# 0.1.4

- include a control that user-provided "." as decimal symbol is turned into "[.]" to be used as regular expression.
- re-enable completion of when values in the header are missing. Values are now completed with their left neighbour, as this is the most frequent use-case when values in the header are omitted.

# 0.1.3

- enable the field 'merge = character(1)' for identifying variables, which would be used when a variable is spread over several columns that need to be combined.

# 0.1.2

initial release to CRAN