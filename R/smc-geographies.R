#' cleaning up cities in a data frame
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
city_clean <- function(data) {
  data1 <- data %>%
    rename(city_orig = city) %>%
    mutate(city_temp = str_replace_all(str_to_lower(city_orig), "\\.|\\-|\\,|\\s", ""),
           city = case_when(str_detect(city_temp, regex("^arther|^ather")) ~ "Atherton",
                                str_detect(city_temp, regex("^belmo|^bellmo|^bemon|blemon")) | city_temp %in% c("bely") ~ "Belmont",
                                str_detect(city_temp, regex("^brisba")) ~ "Brisbane",
                                str_detect(city_temp, regex("^broadmoor")) ~ "Broadmoor",
                                str_detect(city_temp, regex("^burl+in|^burlig|^buling|^buring|^brling|^buriling")) ~ "Burlingame",
                                str_detect(city_temp, regex("^colm|^coma|^colom")) ~ "Colma",
                                str_detect(city_temp, regex("^da.*ity$|^dail|^dlay|^daly.*y$")) ~ "Daly City",
                                str_detect(city_temp, regex("^eap|^epalo|^e.*alto$|^eastpa|paloaltoeast")) ~ "East Palo Alto",
                                str_detect(city_temp, regex("^elgr[ae]n.*da|^elgan.*da")) ~ "El Granada",
                                str_detect(city_temp, regex("^emerald.*ills$")) ~ "Emerald Hills",
                                str_detect(city_temp, regex("^fost|^forst|^fist")) ~ "Foster City",
                                str_detect(city_temp, regex("^halfmo|hmb|^halmo|^hallfmo|^malfmo|^ha.*oonbay$")) ~ "Half Moon Bay",
                                str_detect(city_temp, regex("^hillsbo|^hillbo|^hilsbo|^hissbo|^hiilsbo|^hi.*borough$")) ~ "Hillsborough",
                                str_detect(city_temp, regex("^lahond")) ~ "La Honda",
                                str_detect(city_temp, regex("^ladera")) ~ "Ladera",
                                str_detect(city_temp, regex("^lomam")) ~ "Loma Mar",
                                str_detect(city_temp, regex("^menl|^me[nl]o|menlopark|^men.*park$")) ~ "Menlo Park",
                                str_detect(city_temp, regex("^millb|^milb|^milli|billbrae")) ~ "Millbrae",
                                str_detect(city_temp, regex("^mont[ea]ra")) ~ "Montara",
                                str_detect(city_temp, regex("^mossb|^missb")) ~ "Moss Beach",
                                str_detect(city_temp, regex("^paci.*a$|^pacific$")) ~ "Pacifica",
                                str_detect(city_temp, regex("^pescad|^p.*adero$")) ~ "Pescadero",
                                str_detect(city_temp, regex("^portola|^port.*y$|^port.*lye$")) ~ "Portola Valley",
                                str_detect(city_temp, regex("^princet")) ~ "Princeton-by-the-Sea",
                                str_detect(city_temp, regex("^redwo.*ty$|^red.*city$|^r.*oodcity$|^redwoo|^rwc$|^red wood|^rewoo|redwood$")) ~ "Redwood City",
                                str_detect(city_temp, regex("^sanb[rut]|^s.*bruno$")) ~ "San Bruno",
                                str_detect(city_temp, regex("^sanca")) ~ "San Carlos",
                                str_detect(city_temp, regex("^sangreg")) ~ "San Gregorio",
                                str_detect(city_temp, regex("^sanma[teogry]e|^sm$|^sammat|^s[an].*teo$|^sanmat.*o$|mateo|hillsdale")) ~ "San Mateo",
                                str_detect(city_temp, regex("^s{2,}anfra|^southsanfra|^sosanfra|^ssf$|^sosf$|^southsf$|^sou.*ity$|^s[ou].*isco$")) ~ "South San Francisco",
                                str_detect(city_temp, regex("wood.*de$")) ~ "Woodside",
                                str_detect(city_temp, regex("homeless|unshelt")) ~ "Unsheltered"))
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
