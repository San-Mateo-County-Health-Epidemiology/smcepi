#' Connect to Azure
#'
#' @description
#' Use this function to connect to an Azure database from R. This uses an ODBC driver and the `DBI::dbConnect` function.
#'
#' @usage connect_azure(creds_file,
#'     creds_position = 1,
#'     pwd = rstudioapi::askForPassword("Microsoft password"))
#'
#' @param creds_file This is the path to the .yml file with the Azure information. The .yml must include: server, driver, authenticator, database, uid_suffix and port
#' @param creds_position This is a number that identifies which set of credentials in the .yml file to use. It defaults to 1, which is the first set of credentials. If you have more than one set of credentials in your config file and want to use the second set, you should set `creds_position = 2`
#' @param pwd by default, this allows you to enter your Windows password. You can override this by entering a password in this argument.
#'
#' @return a Microsoft SQL server connection
#' @export
#'
#' @examples
#' \dontrun{
#' # example .yml file format:
#' default:
#'   db:
#'      server: 'server-name'
#'      driver: 'ODBC Driver 18 for SQL Server'
#'      authenticator: 'ActivedirectoryPassword'
#'      database: 'database-name'
#'      uid_suffix: '@organization.org'
#'      port: port-number
#'
#' # connecting with default credentials + windows password:
#' con <- connect_azure(creds_file = "credentials.yml")
#'
#' # connecting with the second set of credentials and a custom password
#' con <- connect_azure(creds_file = "credentials.yml", creds_position = 2, pwd = "password")
#'}
#'
#'
connect_azure <- function(creds_file = "", creds_position = 1, pwd = rstudioapi::askForPassword("Microsoft password")) {

  stopifnot(grepl("*.yml", creds_file) & is.numeric(creds_position))

  creds <- config::get(file = creds_file)[[creds_position]]
  uid <- paste0(Sys.getenv("USERNAME"), creds$uid_suffix)

  con <- DBI::dbConnect(odbc::odbc(),
                        Driver = creds$driver,
                        server = creds$server,
                        Authentication = creds$authenticator,
                        database = creds$database,
                        UID = uid,
                        PWD = pwd,
                        Port = creds$port,
                        timeout = 30)

  return(con)

}

#' Get character variable (varchar) lengths for import into Azure
#'
#' @description
#' Use this to generate a list of varchar(`n`) specifications to be used in an R to Azure import. Each character variable is given a field type of varchar(`n`) where `n` is based on the maximum string length per variable. `n` is 255 for string lengths of 255 or less and is the maximum string length for the variable for any string longer than 255 (ex: `varchar(1001)`)
#'
#' @usage varchar_max(data)
#'
#' @param data a 2x2 dataset that you plan to import into Azure from R
#'
#' @return a named list that specifies the varchar(n) length for each variable
#' @export
#'
#' @importFrom rlang .data
#' @examples
#' data <- data.frame(
#'           col1 = c("blue", "pink", "green", "yellow"),
#'           col2 = c(paste(rep("a", 1000), collapse = ""), "b", "cc", "ddd"))
#'
#' varchar_max(data)
#'
#' # output:
#' #           col1           col2
#' # "varchar(255)" "varchar1000)"
varchar_max <- function(data) {

  varchar <- data %>%
    dplyr::select(tidyselect::where(is.character) | tidyselect::where(is.factor)) %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.factor), ~ as.character(.x))) %>%
    tidyr::pivot_longer(names_to = "var",
                        values_to = "val",
                        values_drop_na = T,
                        cols = tidyselect::everything()) %>%
    dplyr::mutate(char_ct = nchar(.data$val)) %>%
    dplyr::group_by(.data$var) %>%
    dplyr::summarize(max_char_ct = max(.data$char_ct),
                     .groups = "keep") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(max_char_ct = dplyr::case_when(.data$max_char_ct <= 255 ~ 255,
                                                 TRUE ~ .data$max_char_ct),
           varchar = paste0("varchar(", .data$max_char_ct, ")")) %>%
    dplyr::select(-.data$max_char_ct) %>%
    tibble::deframe()

  return(varchar)

}