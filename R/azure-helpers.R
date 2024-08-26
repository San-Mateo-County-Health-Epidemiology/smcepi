#' Connect to Azure
#'
#' @description
#' Use this function to connect to an Azure database from R. This is a wrapper function for the `DBI::dbConnect` function.
#'
#' @usage azure_connect(creds_file,
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
#' con <- azure_connect(creds_file = "credentials.yml")
#'
#' # connecting with the second set of credentials and a custom password
#' con <- azure_connect(creds_file = "credentials.yml", creds_position = 2, pwd = "password")
#'}
#'
#'
azure_connect <- function(creds_file = "", creds_position = 1, pwd = rstudioapi::askForPassword("Microsoft password")) {

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

  # This saves what was typed into R
  code <- c(match.call())

  # this opens the connection pane
  odbc:::on_connection_opened(
    con,
    paste(c("library(smcepi)",
            paste("con <-", gsub(", ", ",\n\t", code))),
          collapse = "\n"))

  return(con)


  return(con)

}

#' Get character variable (varchar) lengths for import into Azure
#'
#' @description
#' Use this to generate a list of `varchar(n)` specifications to be used in an R to Azure import with the `DBI:dbWriteTable()` or `smcepi::azure_import_loop()` functions. This list specifies a `varchar(n)` value for each character variable based on the maximum string length in that variable. `n` is 255 for variables with maximum string lengths of 255 or less and `n` is the maximum string length for variables with maximum string lengths longer than 255.
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

#' Write large datasets from R to Azure
#'
#' @description
#' This function creates a table in Azure from R. It is a wrapper function for the `DBI::dbWriteTable` and the `DBI::dbAppendTable` functions. This function breaks the dataset to be imported into smaller groups and then loops through the groups. The first group is used to create the table and subsequent groups are appended to the table.
#'
#' @param azure_con A DBI Connection object as returned by `dbConnect()`. See help text for `DBI::dbWriteTable` for more details
#' @param data A data.frame object you plan to import
#' @param group_size An integer that indicates the size of the import groups. Ex: a group_size of 50 would split the dataset into groups of 50 records and would then import each group at a time
#' @param table_name A string to specify the name of the table in Azure
#' @param field_types Optional parameter to pass a list of varchar(`n`) lengths for the Azure table. Can be created with the `smcepi::varchar_max()` function
#' @param table_type Optional parameter to specify whether or not you're creating a new table (`table_type = "new"`) or appending data to an existing table (`table_type = "existing"`). The default is `"new"`
#'
#' @return will import the data into azure and will return any errors from the `DBI::dbWriteTable` and the `DBI::dbAppendTable` functions
#' @export
#'
#' @examples
#' \dontrun{
#'
#' azure_import_loop(azure_con = azure_con,
#'                   data = data,
#'                   group_size = 500,
#'                   table_name = "new_azure_table_name",
#'                   field_types = varchar_max)
#'
#'}
azure_import_loop <- function(azure_con, data, group_size, table_name, field_types = NULL, table_type = "new") {

  table_type <- rlang::arg_match(table_type, c("new","existing"))

  data1 <- data %>%
    dplyr::mutate(group = ceiling(dplyr::row_number()/group_size))

  groups <- data1 %>%
    dplyr::distinct(.data$group) %>%
    dplyr::pull(.data$group)

  azure_table <- table_name

  for (i in groups) {

    if (i == 1 & table_type == "new") {

      print("group 1!")

      to_import <- data1 %>%
        dplyr::filter(.data$group == i) %>%
        dplyr::select(-.data$group)

      DBI::dbWriteTable(conn = azure_con,
                   name = azure_table,
                   value = to_import,
                   overwrite = T,
                   field.types = field_types)

    } else {

      print(paste0("group ", i))

      to_import <- data1 %>%
        dplyr::filter(.data$group == i) %>%
        dplyr::select(-.data$group)

      DBI::dbAppendTable(conn = azure_con,
                    name = azure_table,
                    value = to_import,
                    append = T,
                    overwrite = F)

    }
  }
}


