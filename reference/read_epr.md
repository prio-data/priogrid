# Reads the EPR Core 2023 data

Downloads and processes EPR Core (Ethnic Power Relations) data, which
provides information on politically relevant ethnic groups and their
access to state power worldwide. The function formats temporal variables
and adds utility columns for temporal analysis compatible with
PRIO-GRID.

## Usage

``` r
read_epr()
```

## Value

A `tibble` object

## Details

EPR Core tracks the political status of ethnic groups that are
politically relevant in each country-year. This function:

- Downloads the EPR Core dataset from the ETH ICR data repository

- Converts temporal variables (from, to) to proper Date objects
  (gwsdate, gwedate)

- Adds a date_interval utility column for temporal operations

- Returns data in tibble format for analysis

The dataset categorizes ethnic groups based on their access to executive
power, ranging from monopoly and dominance to discrimination and
exclusion. Groups are considered politically relevant if they are either
represented in government, engage in political competition, or face
systematic discrimination.

Political status categories typically include:

- Monopoly: Group holds exclusive power

- Dominance: Group holds dominant power position

- Senior/Junior Partner: Group shares power in coalition

- Powerless: Group lacks political influence

- Discriminated: Group faces systematic discrimination

- Self-exclusion: Group voluntarily excludes itself from politics

## References

Vogt M, Bormann N, Rüegger S, Cederman L, Hunziker P, Girardin K (2015).
“Integrating Data on Ethnicity, Geography, and Conflict: The Ethnic
Power Relations Data Set Family.” *Journal of Conflict Resolution*,
**59**(7), 1327–42.
[2024-10-21](http://prio-data.github.io/priogrid/reference/2024-10-21).

## See also

[`read_geoepr`](http://prio-data.github.io/priogrid/reference/read_geoepr.md)
for spatial ethnic group data,
[`pg_dates`](http://prio-data.github.io/priogrid/reference/pg_dates.md)
for PRIO-GRID temporal coverage

## Examples

``` r
if (FALSE) { # \dontrun{
# Read EPR Core data
epr_data <- read_epr()

# Examine the structure
print(epr_data)
str(epr_data)

# Check available columns
names(epr_data)

# View temporal coverage
range(epr_data$gwsdate, na.rm = TRUE)
range(epr_data$gwedate, na.rm = TRUE)

# Examine unique countries
unique_countries <- unique(epr_data$country)
head(sort(unique_countries), 10)

# Filter to specific country
usa_groups <- epr_data[epr_data$gwid == 2, ]  # USA
print(usa_groups)

# Analyze power status distribution
if("status" %in% names(epr_data)) {
  table(epr_data$status)
}

# Filter groups active in specific year
target_date <- as.Date("2010-01-01")
active_2010 <- epr_data[target_date %within% epr_data$date_interval, ]
nrow(active_2010)

# Analyze temporal changes for specific group
# example_group <- epr_data[epr_data$group == "African Americans", ]
# View(example_group)
} # }
```
