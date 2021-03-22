# ===============
# title: fv-wc.R - (3)
# author: Lucas Burns
# description: More plotting of finviz.com stock headlines
# ===============

library(sentimentr)     # nlp tools
library(tm)             # nlp tools
library(tidytext)       # nlp tools

library(tidyquant)      # manipulation of time
library(wordcloud)      # generate wordcloud
library(dplyr)          # data wrangling/manipulation

library(stringr)        # string manipulation
library(magrittr)       # piping
library(data.table)     # import/export data
library(tidyverse)      # ggplot2, tibble, purr, tidyr
library(tidyquant)      # datetime manipulation

library(ggthemes)       # theme ggplots
library(RColorBrewer)   # expand number of colors in palette
library(reshape2)       # more efficient reshape


# ===============================
# set default theme and palette
ggtheme <- list(
    theme_bw(),
    scale_color_brewer(palette = "Dark2")
)

# set a wrapper for ggplot theme and palette
ncolors <- colorRampPalette(brewer.pal(8, "Dark2"))(11)

ggcust <- function(...){
    ggplot(...) +
      theme_bw() +
      scale_fill_manual(values = ncolors)
}
# ===============================


# ==============
# Importing data
# ==============
sec_df <- as.data.frame(fread("../data/50sec_df.tsv", quote = "", header = TRUE))
sel_df <- as.data.frame(fread("../data/50sel_df.tsv", quote = "", header = TRUE))
tt <- slice_sample(sel_df, n=200)                           # testing dataframe

# Create a custom negate function
"%ni%" <- Negate("%in%")

# =============================================================
# Plot wordcloud grouping based on before or after January 6th
# TODO: Figure out how to give function parameter to change group_by() value
# =============================================================
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
                     use.r.layout = FALSE)
}

wcloud_jan("price")


# ==================================
# Plot general wordcloud word search
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
            scale = c(8, .2))
}

wcloud_wsearch("riot")

# ===============================================
# Plotting sentiment before and after january 6th
# ===============================================
library(janitor)          # cleaning data
library(ggpubr)           # combining plots

temp <- sel_df %>%
          group_by(floor_date(as.POSIXct(.$datetime))) %>%
          unnest_tokens(word, headline) %>%
          merge(get_sentiments("nrc")) %>%
          clean_names() %>%
          dplyr::rename(date = floor_date_as_posi_xct_datetime) %>%
          mutate(sentiment = factor(sentiment, levels = unique(sentiment))) %>%
          count(c("date", "sentiment")) %>%
          mutate(aftjan = if_else(date >= as.POSIXct("2021-01-06"), 1, 0))

temp$date <- null

# Displays frequency of sentiments based on 'aftjan'
p <- temp %>%
         group_by(aftjan, sentiment) %>%
         dplyr::summarize(across(freq, sum)) %>%
         ggplot(aes(x = sentiment, group = aftjan)) +
         geom_col(aes(y = freq, fill = sentiment), show.legend = false) +
         facet_wrap(~ aftjan, nrow = 2, scales = "free")

# Displays count above bars / percent = y
ap <- temp %>%
      ggplot(aes(x = sentiment, group = aftjan)) +
      geom_bar(aes(y = ..prop.., fill = factor(..x..)),stat = "count", show.legend = false) +
      geom_text(aes(label = ..count.., y = ..prop..), stat = "count", vjust = -0.5) +
      labs(y = "percent", fill = "sentiment") +
      facet_wrap(~ aftjan, nrow = 2, scales = "fixed") +
      scale_y_continuous(labels = scales::percent)

# Combine plots
ggarrange(ap, p)


# =================
# Alternative plot: displays percent above bars / percent = y-axis
# =================
p <- ggplot(ii, aes(x=sentiment, group = aftjan)) +
        geom_bar(aes(y = ..prop.., fill = date), stat = "count", show.legend = false) +
        geom_text(aes(label = scales::percent(..prop..),
                      y = ..prop..), stat = "count", vjust = -.5) +
        facet_wrap(~aftjan, nrow = 2, scales = "fixed") +
        scale_y_continuous(labels = scales::percent)


# =====================
# Overall frequencies of sentiments
# =====================
library(grid)
library(gridextra)

aggregate(freq ~ sentiment, temp, sum) %>%
  ggcust(aes(x = sentiment, y = freq, fill = sentiment)) +
  geom_bar(stat = 'identity', show.legend = false)

temp %>%
   group_by(aftjan, sentiment) %>%
   dplyr::summarize(across(freq, sum)) %>%
   spread(sentiment, freq) -> pct

bjan <- round(pct[1,2:ncol(pct)] / rowsums(pct[1,2:ncol(pct)]), 3)
ajan <- round(pct[2,2:ncol(pct)] / rowsums(pct[2,2:ncol(pct)]), 3)

pct <- rbind(pct, cbind(aftjan = 0, bjan), cbind(aftjan = 1, ajan))

grid.table(pct)
# After January 6th, positivity, joy, and trust decreased
# negativity, disgust, sadness, fear, agner, anticipation, and surprise increased


# ============================
# Sentiment by word and sector
# ============================
get_secsent <- function(term) {
  sel_df %>%
      filter(grepl(term, headline)) %>%
      mutate(floor_date = floor_date(as.POSIXct(datetime))) %>%
      sentimentr::get_sentences() %$%
      sentiment_by(., by = c("headline", "floor_date", "sector")) %>%
      .[, -c("sd")] %>%
      .[ave_sentiment != 0] -> secsent

  ggcust(secsent, aes(floor_date, ave_sentiment)) +
    geom_point(aes(size = word_count, color = sector)) +
    geom_hline(yintercept = 0) +
    geom_smooth(linetype = 0)
}

get_secsent("capitol")

# ========
# Stemming
# ========
# Unused libraries (so far)
library(tokenizers)
library(syuzhet)
library(qdap)           # nlp tools


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
