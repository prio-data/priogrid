# Calculate PRIO-GRID variables

Calculates PRIO-GRID variables based on current config and saves them to
disk. Each variable is computed by calling its corresponding gen\_\*()
function and saved as an .rds file in the custom data folder.

## Usage

``` r
calc_pg(varnames = NULL, overwrite = FALSE, config = pg_current_config())
```

## Arguments

- varnames:

  Character vector with variable names from
  [pgvariables](http://prio-data.github.io/priogrid/reference/pgvariables.md).
  If NULL (default), calculates all available variables.

- overwrite:

  Logical. If FALSE (default), skips variables that already exist in the
  output folder. If TRUE, recalculates all specified variables.

## Value

NULL (invisibly). Called for side effects (saving files).

## Examples

``` r
if (FALSE) { # \dontrun{
  # Calculate single variable
  calc_pg("ne_disputed_area_share")
  r <- load_pgvariable("ne_disputed_area_share")

  # Calculate all variables
  calc_pg()

  # Recalculate existing variables
  calc_pg(overwrite = TRUE)
} # }
```
