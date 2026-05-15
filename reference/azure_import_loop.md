# Write large datasets from R to Azure

This function creates a table in Azure from R. It is a wrapper function
for the
[`DBI::dbWriteTable`](https://dbi.r-dbi.org/reference/dbWriteTable.html)
and the
[`DBI::dbAppendTable`](https://dbi.r-dbi.org/reference/dbAppendTable.html)
functions. This function breaks the dataset to be imported into smaller
groups and then loops through the groups. The first group is used to
create the table and subsequent groups are appended to the table.

## Usage

``` r
azure_import_loop(
  azure_con,
  data,
  group_size,
  table_name,
  field_types = NULL,
  table_type = "new"
)
```

## Arguments

- azure_con:

  A DBI Connection object as returned by `dbConnect()`. See help text
  for
  [`DBI::dbWriteTable`](https://dbi.r-dbi.org/reference/dbWriteTable.html)
  for more details

- data:

  A data.frame object you plan to import

- group_size:

  An integer that indicates the size of the import groups. Ex: a
  group_size of 50 would split the dataset into groups of 50 records and
  would then import each group at a time

- table_name:

  A string to specify the name of the table in Azure

- field_types:

  Optional parameter to pass a list of varchar(`n`) lengths for the
  Azure table. Can be created with the
  [`smcepi::varchar_max()`](https://san-mateo-county-health-epidemiology.github.io/smcepi/reference/varchar_max.md)
  function

- table_type:

  Optional parameter to specify whether or not you're creating a new
  table (`table_type = "new"`) or appending data to an existing table
  (`table_type = "existing"`). The default is `"new"`

## Value

will import the data into azure and will return any errors from the
[`DBI::dbWriteTable`](https://dbi.r-dbi.org/reference/dbWriteTable.html)
and the
[`DBI::dbAppendTable`](https://dbi.r-dbi.org/reference/dbAppendTable.html)
functions

## Examples

``` r
if (FALSE) { # \dontrun{

azure_import_loop(azure_con = azure_con,
                  data = data,
                  group_size = 500,
                  table_name = "new_azure_table_name",
                  field_types = varchar_max)

} # }
```
