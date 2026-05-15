# Get file path for a file stored in a folder with multiple versions of a file

If you have multiple versions of the same data set stored in a folder,
this will pull the newest or oldest file path. You can enter a path
and/or a pattern to look for.

## Usage

``` r
get_file_path(directory = getwd(),
             pattern = NULL,
             sort_method = "created date",
             sort_type = "newest",
             include_directories = FALSE)
```

## Arguments

- directory:

  a string specifying the directory in which you want to look. It
  defaults to the working directory

- pattern:

  a string and/or regular expression specifying which files you're
  interested in

- sort_method:

  - `"created date"` (the default): sort by the date the file was
    created

  - `"modified date"`: sort by the date the file was last modified

  - `"accessed date"`: sort by the date the file was last accessed

- sort_type:

  - `"newest"` (the default): get the newest file based on sort method

  - `"oldest"`: get the oldest file based on sort method

- include_directories:

  boolean to indicate whether or not directories should be included in
  your results

## Value

the file path as a string

## Examples

``` r
if (FALSE) { # \dontrun{

# get the file that was most recently accessed in a directory
get_file_path(sort_method = "accessed date", sort_type = "newest")

# get the first file that was created in a different directory
get_file_path(directory = "data", sort_type = "newest")

# get the most recently modified parquet file in a directory
get_file_path(directory = "data", pattern = "*.parquet",
              sort_method = "modified date", sort_type = "newest")

} # }
```
