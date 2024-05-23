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
