get_file <- function(path = NULL, pattern = NULL, sort_method, sort_type = "newest") {

  # get the files that match the path + pattern -----------------
  files <- file.info(list.files(path = path,
                                pattern = pattern,
                                full.names = T))

  # ensure a valid sort method was inputted ---------------------
  sort_method <- rlang::arg_match(sort_method, c("ctime","mtime", "atime"))

  switch(sort_type,
         newest = {sort_type = TRUE},
         oldest = {sort_type = FALSE})

  files["sort_col"] <- files[[sort_method]]

  rownames(files[order(files$sort_col, decreasing = sort_type),][1,])

}
