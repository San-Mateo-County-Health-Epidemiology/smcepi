# set up the inputs
highlight_colors <- c("006cb6")
highlight_text <- c("Brown")
title <- "Brown and blue eyes were most common"

# split the title into pieces with the breakpoints as the highlighted words ----
list <- list()
ct <- 0
text_ct <- 0
for(text in highlight_text) {

  text_ct <- text_ct + 1

  first <- str_split_fixed(title, text, 2)[1]
  title <- str_split_fixed(title, text, 2)[2]
  print(first)
  print(text)

  ct <- ct + 1
  list[[ct]] <- first

  ct <- ct + 1
  list[[ct]] <- text

  if(text_ct == length(highlight_text)) {

    ct <- ct + 1
    list[[ct]] <- title

  }

}

words_list <- unlist(list)

# add the colors to the highlighted words
list_ct <- 0
highlight_ct <- 0

colors_list <- words_list

for(word in words_list) {

  list_ct <- list_ct + 1
  print(word)

  if(word %in% highlight_text) {

    highlight_ct <- highlight_ct + 1

    colors_list[list_ct] <- paste0("<span style='color:#", highlight_colors[highlight_ct], ";'>", word, "</span>")

  }

}

colors_list

title <- paste(colors_list, collapse = "")


eye_plot <- eye_freq %>%
  ggplot(aes(x = Eye,
             y = freq,
             fill = Eye)) +
  scale_fill_manual(values = c("Brown" = "#006cb6",
                               "Blue" = "#D5D8DC",
                               "Hazel" = "#D5D8DC",
                               "Green" = "#D5D8DC")) +
  geom_bar(position = "dodge",
           stat = "identity") +
  labs(title = title) +
  # labs(title = "<span style='color:#006cb6;'>Brown</span> eyes were the most common in this sample")+
  theme_gg_smc()
eye_plot
