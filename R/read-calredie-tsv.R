#' Using `read_tsv_cr`
#'
#' @description
#' This is a wrapper for `read.csv2` that is meant specifically for CalREDIE tab separated files from the DDP.
#'
#' @usage read_tsv_cr(file)
#'
#' @param file this should be a string
#'
#' @return a data frame
#' @export
#'
#' @examples
#' \dontrun{
#' data <- read_tsv_cr("file.tsv")
#'}
read_tsv_cr <- function(file) {

  stopifnot(class(file) == "character", grepl("*.tsv$", file))

  if(!grepl("*.tsv$", file)) {

    stop("The file is not a .tsv. This is meant for CalREDIE DDP tab separated files.")
  }

  data <- utils::read.csv2(file,
                    header = T,
                    sep = "\t",
                    stringsAsFactors = FALSE,
                    quote = "", na = c(""))

  data

}
