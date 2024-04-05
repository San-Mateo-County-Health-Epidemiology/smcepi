#' Title
#'
#' @param title_text
#' @param highlight_words
#' @param highlight_colors
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom stringr str_split_fixed
gg_color_title <- function(title_text, highlight_words, highlight_colors) {

  if(length(highlight_words) != length(highlight_colors)) {

    print("The length of the highlight_words and highlight_colors vectors must match.")

  }

  # split the title into pieces with the highlighted words as break points ----
  ## inputs ----
  list <- list()
  ct <- 0
  text_ct <- 0

  ## loop to split text ----
  for(text in highlight_words) {

    text_ct <- text_ct + 1

    first <- stringr::str_split_fixed(title_text, text, 2)[1]
    title_text <- stringr::str_split_fixed(title_text, text, 2)[2]

    ct <- ct + 1
    list[[ct]] <- first

    ct <- ct + 1
    list[[ct]] <- text

    if(text_ct == length(highlight_words)) {

      ct <- ct + 1
      list[[ct]] <- title_text

    }

  }

  words_list <- unlist(list)

  # add the colors to the highlighted words ----
  ## inputs -----
  list_ct <- 0
  highlight_ct <- 0

  colors_list <- words_list

  ## loop to assign colors ----
  for(word in words_list) {

    list_ct <- list_ct + 1

    if(word %in% highlight_words) {

      highlight_ct <- highlight_ct + 1

      colors_list[list_ct] <- paste0("<span style='color:#", highlight_colors[highlight_ct], ";'>", word, "</span>")

    }

  }

  title <- paste(colors_list, collapse = "")
  title

}


