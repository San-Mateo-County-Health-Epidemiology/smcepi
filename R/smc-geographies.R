#' cleaning up San Mateo Cities in a data frame
#'
#' @description
#' This function is meant to be used to clean up San Mateo cities in a dataframe. It will only look for San Mateo County cities - it doesn't look for other cities. Non-San Mateo cities will return an `NA` in the `city_clean` column.
#'
#' @usage city_clean(data,
#'     city_col,
#'     new_col)
#'
#' @param data This is the name of the dataframe with the city variable in it that you'd like to clean
#' @param city_col This is a string that specifies the name of the city variable you want to clean
#' @param new_col (optional): This is a string to specify the name of the variable with the cleaned city names. By default the cleaned cities will be saved in a variable called `city_clean`
#'
#' @return a dataset with a new variable for the cleaned city values
#' @export
#'
#' @examples
#' \dontrun{
#'  data1 <- data %>%
#'     smc_city_clean(city_col = "city_dirty",
#'                new_col = "city_clean")
#'
#'}
#'
#'
smc_city_clean <- function(data, city_col = "city", new_col = "city_clean") {
  data1 <- data %>%
    dplyr::rename(city_orig = !!city_col) %>%
    dplyr::mutate(city_temp = stringr::str_replace_all(stringr::str_to_lower(city_orig), "\\.|\\-|\\,|\\s", ""),
                  city = stringr::str_replace_all(city_temp,
                                                  c("^arther.*$|^ather.*$" = "Atherton",
                                                    "^belmo.*$|^bellmo.*$|^bemon.*$|^blemon.*$|^bely$" = "Belmont",
                                                    "^brisba.*$" = "Brisbane",
                                                    "^broadmoor.*$" = "Broadmoor",
                                                    "^burl+in.*$|^burlig.*$|^buling.*$|^buring.*$|^brling.*$|^buriling.*$" = "Burlingame",
                                                    "^colm.*$|^coma.*$|^coloma.*$" = "Colma",
                                                    "^da.*ity$|^dail.*$|^dlay.*$|^daly.*y$" = "Daly City",
                                                    "^eap.*$|^epalo.*$|^e.*alto$|^eastpa.*$|^paloaltoeast.*$" = "East Palo Alto",
                                                    "^elgr[ae]n.*da$|^elgan.*da$" = "El Granada",
                                                    "^emerald.*ills$" = "Emerald Hills",
                                                    "^fost.*$|^forst.*$|^fist.*$" = "Foster City",
                                                    "^halfmo.*$|hmb.*$|^halmo.*$|^hallfmo.*$|^malfmo.*$|^ha.*oonbay$" = "Half Moon Bay",
                                                    "^hillsbo.*$|^hillbo.*$|^hilsbo.*$|^hissbo.*$|^hiilsbo.*$|^hi.*borough$" = "Hillsborough",
                                                    "^lahond.*$" = "La Honda",
                                                    "^ladera.*$" = "Ladera",
                                                    "^lomam.*$" = "Loma Mar",
                                                    "^menl.*$|^me[nl]o.$|^menlopark$|^men.*park$" = "Menlo Park",
                                                    "^millb.*$|^milb.*$|^milli.*$|^.*billbrae.*$" = "Millbrae",
                                                    "^mont[ea]ra.*$" = "Montara",
                                                    "^mossb.*$|^missb.*$" = "Moss Beach",
                                                    "^paci.*a$|^pacific$" = "Pacifica",
                                                    "^pescad.*$|^p.*adero$" = "Pescadero",
                                                    "^portola.*$|^port.*y$|^port.*lye$" = "Portola Valley",
                                                    "^princet.*$" = "Princeton-by-the-Sea",
                                                    "^redwo.*ty$|^red.*city$|^r.*oodcity$|^redwoo.*$|^rwc$|^redwood.*$|^rewoo.*$|^redwood$" = "Redwood City",
                                                    "^sanb[rut].*$|^s.*bruno$" = "San Bruno",
                                                    "^sanca.*s$" = "San Carlos",
                                                    "^sangreg.*$" = "San Gregorio",
                                                    "^sanma[teogry]eo.*$|^sanma[teogry]e.*$|^sm$|^sammat.*$|^s[an].*teo$|^sanmat.*o$|^.*mateo.*$|^.*hillsdale.*$" = "San Mateo",
                                                    "^s{2,}anfra.*$|^southsanfra.*$|^so{0,1}sa{0,1}n{0,1}fra.*$|^ssf$|^sosf$|^southsf$|^sou.*ity$|^s[ou].*isco$" = "South San Francisco",
                                                    "^wood.*de$" = "Woodside",
                                                    "^.*homeless.*$|^.*unshelt.*$" = "Unsheltered")),
                  city = dplyr::case_when(!stringr::str_detect(city, "[A-Z]") ~ NA_character_,
                                          TRUE ~ city)) %>%
    dplyr::rename(!!new_col := city,
                  !!city_col := city_orig) %>%
    dplyr::select(-city_temp)

  data1

}

#' Classify zip codes by region in San Mateo County
#'
#' @description
#' This function sorts 5-digit San Mateo zip codes into county regions: north, south, mid, coastside and "not a residential zip". Non-San Mateo zipcodes will return an `NA` in the `city_clean` column.
#'
#' @usage city_clean(data,
#'     city_col,
#'     new_col)
#'
#' @param data This is the name of the dataframe with the zip code variable in it that you'd like to sort
#' @param zip_col (optional): This is a string that specifies the name of the zip code variable you want to sort. By default the function assumes the zipcodes are stored in a variable called `zip`
#' @param region_col (optional): This is a string to specify the name of the variable with the zip regions. By default the sorted zipcodes will be saved in a variable called `region_col`
#'
#' @return a dataset with a new variable for the sorted zip codes
#' @export
#'
#' @examples
#' \dontrun{
#'  data1 <- data %>%
#'     smc_zip_region_sort(zip_col = "zip",
#'                         region_col = "zip_region")
#'
#'}
#'
#'
smc_zip_region_sort <- function(data, zip_col = "zip", region_col = "zip_region") {

  data1 <- data %>%
    dplyr::rename(zip = !!zip_col) %>%
    dplyr::mutate(zip_region = dplyr::case_when(
      zip %in% c("94005", "94014", "94015", "94030", "94044", "94066", "94080") ~ "North",
      zip %in% c("94002", "94010", "94065", "94070", "94401", "94402", "94403", "94404") ~ "Mid",
      zip %in% c("94025", "94027", "94028", "94061", "94062", "94063", "94303") ~ "South",
      zip %in% c("94019", "94020", "94021", "94038", "94060", "94074") ~ "Coast",
      zip %in% c("94017", "94018", "94037", "94064", "94128") ~ "Not a residential zip")) %>%
    dplyr::rename(!!region_col := zip_region,
                  !!zip_col := zip)

  data1

}
