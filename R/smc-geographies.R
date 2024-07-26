#' cleaning up cities in a data frame
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
city_clean <- function(data, city_col) {
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
                                                    "^s{2,}anfra.*$|^southsanfra.*$|^sosanfra.*$|^ssf$|^sosf$|^southsf$|^sou.*ity$|^s[ou].*isco$" = "South San Francisco",
                                                    "^wood.*de$" = "Woodside",
                                                    "^.*homeless.*$|^.*unshelt.*$" = "Unsheltered")))
  data1

}


#' Assigning county regions based on zip codes
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
zip_region_function <- function(data) {
  data1 <- data %>%
    mutate(zip_region = case_when(
      zip %in% c("94005", "94014", "94015", "94030", "94044", "94066", "94080") ~ "North",
      zip %in% c("94002", "94010", "94065", "94070", "94401", "94402", "94403", "94404") ~ "Mid",
      zip %in% c("94025", "94027", "94028", "94061", "94062", "94063", "94303") ~ "South",
      zip %in% c("94019", "94020", "94021", "94038", "94060", "94074", "94109") ~ "Coast"))

  data1

}
