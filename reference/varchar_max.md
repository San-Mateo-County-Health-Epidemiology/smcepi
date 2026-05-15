# Get character variable (varchar) lengths for import into Azure

Use this to generate a list of `varchar(n)` specifications to be used in
an R to Azure import with the `DBI:dbWriteTable()` or
[`smcepi::azure_import_loop()`](https://san-mateo-county-health-epidemiology.github.io/smcepi/reference/azure_import_loop.md)
functions. This list specifies a `varchar(n)` value for each character
variable based on the maximum string length in that variable. `n` is 255
for variables with maximum string lengths of 255 or less and `n` is the
maximum string length for variables with maximum string lengths longer
than 255.

## Usage

``` r
varchar_max(data)
```

## Arguments

- data:

  a 2x2 dataset that you plan to import into Azure from R

## Value

a named list that specifies the varchar(n) length for each variable

## Examples

``` r
data <- data.frame(
          col1 = c("blue", "pink", "green", "yellow"),
          col2 = c(paste(rep("a", 1000), collapse = ""), "b", "cc", "ddd"))

varchar_max(data)
#>            col1            col2 
#>  "varchar(255)" "varchar(1000)" 

# output:
#           col1           col2
# "varchar(255)" "varchar1000)"
```
