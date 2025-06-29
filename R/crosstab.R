#' @import assertthat

# CONSTANTS
CT_CLASS <- "crosstab"

# CONSTRUCTORS
new_crosstab <- function() {
    # Structure, main data is output table

    # Attributes:
    # Data table (subtable)
    # List of previous data tables, named after their variable_name
}

# Don't store intermediate data to go searching for, just create getters
# Some functions will use polymorphism, like calculating the total row will
#   be different for a subtable, and mean/sd will still work on likert data
# Figure out the best way to handle likert data, like where to store mappings
#   (probably another type of subtable)
