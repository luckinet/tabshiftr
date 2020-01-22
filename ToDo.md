# high priority:

- figure out all combinations of variable combinations. I now see more and more examples where it is not perfectly clear what should be done to a table, I'll collect those cases here:
  1. a variable is wide but limited to a subset of columns. At the same time, the table contains a long column containing another variable. While this is a normal case of a wide variable, the wide variable may be somewhere "in the middle of the table", i.e., there may be unneeded columns between the wide and the long table. So far, this can't be handled.
  2.
- make tests for "makeSchema()"

# medium priority:

- needs a function that exports schema descriptions into a range of formats (xml, json) that are typically used to store those information.
- I guess it makes sense to waste a thought or two on simplifying the reorganise function into subfunctions. Those could be:
  a) a function to identify and iterate through clusters. This function would extract from schema information and provide the cluster-specific subset to the following function.
  b) a function that identifies "reshape operations". This function would extract from the schema information which variables need to be treated how (gather, spread, split).
  c) a function that manages column names.
  d) a function for each of the table operations gather, spread and split. Obviously those do already exist in the tidyverse, but the table operations we need here go beyond the functioning of those dplyr-functions, so the functions I am proposing here would be a specific wrapper of those tidyverse functions.
  e) ...

# low priority:

- parallelise processing of variables
-