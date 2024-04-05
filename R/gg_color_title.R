#' Using `gg_color_title()`
#'
#' @description
#' This function allows you to selectively color words in the text of your `ggplot2` plot. In order for this to work, you'll need to use the `gg_text::element_markdown()` in the `ggplot2::theme` for the text you want to color.
#'
#' @param title_text This is a string that is the entire title (or subtitle or caption)
#' @param highlight_words This is a vector of the words you want to highlight from the `title_text` string. This vector must be the same length as the `highlight_colors` vector
#' @param highlight_colors This is a vector of the colors you want for each word in the `highlight_words` argument. The first color corresponds to the first word, the second color with the second word, etc. This vector must be the same length as the `highlight_words` vector
#'
#' @return # a string that can be used for a ggtext::element_markdown() title
#' @export
#'
#' @examples
#'title <- gg_color_title("Brown and blue eyes are most common",
#'                        c("Brown", "blue"), c("873600", "006cb6"))
#'HairEyeColor %>%
#'  data.frame() %>%
#'  dplyr::group_by(Eye) %>%
#'  dplyr:: summarize(freq = sum(Freq),
#'  .groups = "keep") %>%
#'  dplyr:: ungroup() %>%
#'  ggplot2::ggplot(aes(x = Eye,
#'  y = freq,
#'  fill = Eye)) +
#'  geom_bar(position = "dodge",
#'  stat = "identity") +
#'  labs(title = title) +
#'  ggplot2::theme(plot.title = ggtext::element_markdown(size = 16, hjust = 0, face = "bold"))
#'
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


