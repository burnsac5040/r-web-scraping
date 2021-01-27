library(sentimentr)     # nlp tools
library(tm)             # nlp tools
library(tidytext)       # nlp tools
library(qdap)           # nlp tools

library(tidyquant)      # manipulation of time
library(wordcloud)      # generate wordcloud
library(dplyr)          # data wrangling/manipulation
library(stringr)        # string manipulation
library(magrittr)       # piping
library(data.table)     # import/export data
library(tidyverse)      # ggplot2, tibble, purr, tidyr

library(ggthemes)       # theme ggplots
library(RColorBrewer)   # expand number of colors in palette
library(reshape2)       # more efficient reshape

# Importing data
# ==============
sec_df <- as.data.frame(fread("data/50sec_df.tsv", quote = "", header = TRUE))
sel_df <- as.data.frame(fread("data/50sel_df.tsv", quote = "", header = TRUE))
tt <- slice_sample(sel_df, n=200) # Testing dataframe


"%ni%" <- Negate("%in%")

# Plot wordcloud grouping based on before or after January 6th
# NOTE: Cannot figure out how to give function parameter to change group_by() value
# ================================================================================
wcloud_jan <- function(phrase){
  layout(matrix(c(1, 2), nrow = 2), heights = c(1, 4))
  par(mar = rep(0, 4))
  plot.new()
  text(x = 0.5, y = 0.5, phrase)

  sel_df %>%
    filter(grepl(phrase, headline)) %>%
    group_by(aftjan) %>%
    unnest_tokens("word", headline) %>%
    filter(word %ni% stopwords('en') & word %ni% phrase) %>%
    add_count(aftjan, word) %>%
    acast(word ~ aftjan, value.var = "n", fill = 0) %>%
    comparison.cloud(max.words = 50, random.order = FALSE,
                     colors = c("lightskyblue", "darkorchid1"),
                     main = "Title", use.r.layout = FALSE)
}

wcloud_jan("price")

# Plot general wordcloud word search
# NOTE: Not sure if main = "Title" is required - works without it
# ==================================
wcloud_wsearch <- function(phrase){
  layout(matrix(c(1, 2), nrow = 2), heights = c(1, 4))
  par(mar = rep(0, 4))
  plot.new()
  text(x = 0.5, y = 0.5, phrase)

  temp <- sel_df %>%
    filter(grepl(phrase, headline)) %>%
    unnest_tokens("word", headline) %>%
    filter(word %ni% stopwords("en") & word %ni% phrase) %>%
    add_count(word, sort = TRUE) %>%
    .[!duplicated(.[c("word", "n")]),]

  # png("wc-riot.png", width = 1280, height = 800, units = "in", res = 300)
  wordcloud(temp$word, freq = temp$n, max.words = 250,
            random.order = FALSE, color = brewer.pal(8, "Dark2"),
            main = "Title", scale = c(8, .2))
}

wcloud_wsearch("riot")

#####################################################
##################### Stemming ######################
#####################################################

xxx <- slice_sample(pos, n=30)
st <- xxx[['headline']]

get_wordlist <- function(list_){
  wordlist <- c()
  for (line_ in list_){
    words_ <- words(line_)
    for (word_ in words_){
      wordlist <- append(wordlist, word_)
    }
  }
  return(unique(wordlist))
}

get_stemmed <- function(wordlist){
  for (i in c(1:length(wordlist))){
    wordlist[i] <- wordStem(String(wordlist[i]))
  }
  return(wordlist)
}
