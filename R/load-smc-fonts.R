#' Download SMC Fonts
#'
#' @description This function uses the 'extrafont' package to download and enable Arial and Georgia which are the official fonts for the SMC style guide.
#'
#' @return this will enable all of the `extrafont` fonts on your computer.
#' @export
#'
#' @examples
#' # just run the function empty
load_smc_fonts <- function() {

  extrafont::loadfonts(quiet = T)

  if(sum(grepl("^Arial$", names(grDevices::windowsFonts()))) == 0 | sum(grepl("^Georgia$", names(grDevices::windowsFonts()))) == 0) {

    print("You need to install the Arial and Georgia fonts. Press 'y' to continue.")

    extrafont::font_import()
    extrafont::loadfonts(quiet = T)

  } else {

  }

}
