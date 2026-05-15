# Reading large tab separated files with `read_tsv_cr`

This is a wrapper for `read.csv2` that is meant for large tab separated
values, including CalREDIE tab separated files from the DDP.

## Usage

``` r
read_tsv_cr(file)
```

## Arguments

- file:

  the file path of the .tsv file

## Value

a data frame

## Examples

``` r
if (FALSE) { # \dontrun{
data <- read_tsv_cr("file.tsv")
} # }
```
