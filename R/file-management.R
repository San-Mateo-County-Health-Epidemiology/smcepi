#' Get file path for a file stored in a folder with multiple versions of a file
#'
#' @description
#' If you have multiple versions of the same data set stored in a folder, this will pull the newest or oldest file path. You can enter a path and/or a pattern to look for.
#'
#' @usage get_file_path(path = NULL,
#'              pattern = NULL,
#'              sort_method = "created date",
#'              sort_type = "newest")
#'
#' @param path a string specifying the directory in which you want to look. If you're already in the directory, you can leave this blank
#' @param pattern a string and/or regular expression specifying which files you're interested in
#' @param sort_method
#'   * `"created date"` (the default): sort by the date the file was created
#'  * `"modified date"`: sort by the date the file was last modified
#'  * `"accessed date"`: sort by the date the file was last accessed
#' @param sort_type
#'   * `"newest"` (the default): get the newest file based on sort method
#'  * `"oldest"`: get the oldest file based on sort method
#'
#' @return the file path as a string
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # get the file that was most recently accessed in a directory
#' get_file_path(sort_method = "accessed date", sort type = "newest")
#'
#' # get the first file that was created in a different directory
#' get_file_path(path = "data", sort_type = "newest")
#'
#' # get the most recently modified parquet file in a directory
#' get_file_path(path = "data", pattern = "*.parquet",
#'               sort_method = "modified date", sort_type = "newest")
#'
#'}
get_file_path <- function(path = NULL, pattern = NULL, sort_method = "created date", sort_type = "newest") {

  # get the files that match the path + pattern -----------------
  files <- file.info(list.files(path = path,
                                pattern = pattern,
                                full.names = T))

  # ensure a valid sort method was inputted ---------------------
  sort_method <- rlang::arg_match(sort_method, c("created date","modified date", "accessed date"))

  switch(sort_method,
         `created date` = {sort_method = "ctime"},
         `modified date` = {sort_method = "mtime"},
         `accessed date` = {sort_method = "atime"})

  # set decreasing variable based on sort type ------------------
  sort_type <- rlang::arg_match(sort_method, c("newest","oldest"))

  switch(sort_type,
         newest = {sort_type = TRUE},
         oldest = {sort_type = FALSE})

  # sort data + pull file path ----------------------------------
  files["sort_col"] <- files[[sort_method]]
  rownames(files[order(files$sort_col, decreasing = sort_type),][1,])

}
