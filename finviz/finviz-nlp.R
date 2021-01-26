# -------------------
# title: finviz-nlp.R
# author: Lucas Burns
# description: Sentiment analysis on finviz.com stock headlines
# -------------------

library(rvest)
library(httr)
library(dplyr)
library(stringr)
library(magrittr)
library(BatchGetSymbols)
library(flipTime)
library(data.table)
library(tidyverse)
# install.packages('Rstem', repos = "http://www.omegahat.net/R")
# devtools::install_github("timjurka/sentiment/sentiment")

# ---------------
# Getting tickers
# ---------------

sp500 <- GetSP500Stocks()
sp500_tickers <- sp500[, c("Tickers", "Company", "GICS.Sector")]

samp_sp500 <- slice_sample(sp500_tickers, n=250)
samp_sp500 <- samp_sp500[grep("\\.", samp_sp500, invert = TRUE)]
# samp_sp500 <- samp_sp500[!grepl("\\.", samp_sp500$Tickers), ]

samp_tick <- samp_sp500$Tickers

base_url <- "https://finviz.com/quote.ashx?t="

get_newstable <- function(tickers){
    all_dfs <- list()

    for (ticker in tickers){
        skip <- FALSE

        # catch error if page or table on page doesn't exist
        tryCatch({
            page <- read_html(paste0(base_url, ticker[[1]]))
            newstable <- html_nodes(page, "[id='news-table']") %>%
                                                html_table(fill = TRUE)
        },
        error = function(e) {skip <<- TRUE}
        )

        if (skip) { next }

        news_df <- data.frame(newstable)
        dfs <- list()

        for (i in 1:dim(news_df)[1]){
            date_ <- news_df[i, 1]
            head_ <- news_df[i, 2]
            date_split <- unlist(strsplit(date_, " "))

            if (length(unlist(date_split)) == 1){
                ptime <- date_split[[1]]
            }else{
                ptime <- date_split[[2]]
                pdate <- date_split[[1]]
            }

            temp <- data.frame(ticker, pdate, ptime, head_)
            dfs[[i]] <- temp

        }
        all_dfs <- append(all_dfs, dfs)
        print(str_glue("Completed ticker: {ticker}"))
        Sys.sleep(3)
    }
    return(all_dfs)
}

parsed_dfs <- get_newstable(samp_tick)
pnews_df <- do.call(rbind, parsed_dfs)

# -----------------
# Cleaning the data
# -----------------

# NOTE: Alternative ways to read in a .tsv file
# pnews_df <- read.table(file = "data/pnews_df.tsv", sep = "\t", header = TRUE)

pnews_df <- as.data.frame(fread("data/50pnews_df-dirty.tsv", quote = "", header = TRUE))
sp500_df <- as.data.frame(fread("data/50sector_df.tsv", quote = ""))

colnames(pnews_df) <- c("ticker", "date", "time", "headline")
# pnews_df <- pnews_df[,-1]
# pnews_df <- pnews_df[, !(names(pnews_df) %in% c("pdatetime"))]

pnews_df$datetime_ <- paste(pnews_df$date, pnews_df$time)
pnews_df$datetime <- AsDateTime(pnews_df$datetime_, us.format = TRUE)
pnews_df$datetime_ <- NULL

pnews_df$headline %<>% tolower()

# pnews <- copy(pnews_df)

pnews_df$headline <- gsub("[[:blank:]][[:alpha:]]*\\.com$", "", pnews_df$headline) # Deletes website.com
# pnews_df$headline <- gsub("\\s*\\w*$", "", pnews_df$headline) # Deletes last word

pnews_df$headline <- gsub("(investorplace|yahoo finance|motley fool|zacks|reuters|insider monkey|business wire|marketwatch|smarteranalyst|benzinga|globenewswire|techcrunch|quartz|bloomberg|investopedia|newswire|tipranks|gurufocus.com)", "", pnews_df$headline)
pnews_df$headline <- gsub("([[:punct:]]|[[:digit:]])", "", pnews_df$headline)
pnews_df$headline <- gsub("\\bpr\\b", "", pnews_df$headline)
pnews_df$headline <- str_squish(pnews_df$headline)


####################################################
###################### NLP #########################
####################################################

news_df <- copy(pnews_df)

# Search for keywords
# NOTE: Alternative way to search for a keyword, however the above picks up more
# riot <- aftjan[aftjan$headline %like% "riot", ]
riot <- news_df %>% filter(str_detect(news_df$headline, "riot"))
capitol <- news_df %>% filter(str_detect(news_df$headline, "capitol"))
qanon <- news_df %>% filter(str_detect(news_df$headline, "qanon"))
war <- news_df %>% filter(str_detect(news_df$headline, "war"))

# write.table(pnews_df, file="data/pnews_df.tsv", quote = FALSE, sep="\t", row.names = FALSE)
# write.csv(pnews_df, file = "data/pnews_df.csv")

#####################################################
################ Polarity / Emotion #################
#####################################################

library(sentiment)      # Rstem, NLP, tm are required
library(wordcloud)
library(sentimentr)
library(tidytext)
library(tidyquant)      # Manipulation of time

tidytext::stop_words
tidytext::sentiments

# Classify polarity and emotion
polarity_df <- classify_polarity(pnews_df$headline, algorithm = "bayes")
emotion_df <- classify_emotion(pnews_df$headline, algorithm = "bayes", prior = 1.0)

# Read back in the dataframes
pnews_df <- as.data.frame(fread("data/50pnews_df.tsv", quote = "", header = TRUE))
polarity_df <- as.data.frame(fread("data/50polarity_df.tsv", quote = "", header = TRUE))
emotion_df <- as.data.frame(fread("data/50emotion_df.tsv", quote = "", header = TRUE))

colnames(polarity_df)[colnames(polarity_df) == "BEST_FIT"] <- "pol_best"
colnames(polarity_df)[colnames(polarity_df) == "POS/NEG"] <- "pos_neg"
colnames(emotion_df)[colnames(emotion_df) == "BEST_FIT"] <- "emo_best"

# Create a larger combined dataframe
emopol_df <- cbind(pnews_df, polarity_df, emotion_df)
emopol_df <- emopol_df %>% mutate_at('emo_best', ~replace(., is.na(.), 'unknown'))

pos <- emopol_df[emopol_df$pol_best == 'positive', ]
neg <- emopol_df[emopol_df$pol_best == 'negative', ]

print(glue("pos: {dim(pos)}, neg: {dim(neg)}"))

# Turning best fit emotion to categories
sel_df <- select(emopol_df, 1, 4, 5, 8:9, 16) %>% as.data.frame()

colnames(sel_df)[colnames(sel_df) == "POS/NEG"] <- "pol_score"

emo_df <- within(sel_df,
    emo_best <- factor(emo_best, levels = names(table(emo_best))),
    pol_best <- factor(pol_best, levels = names(table(pol_best))))

#####################################################
##################### Plotting ######################
#####################################################

emo_df <- as.data.frame(fread("data/50emo_df.tsv", quote = "", header = TRUE))

library(ggthemes)           # Theme ggplots
library(RColorBrewer)       # Expand number of colors in palette
library(ggplot2)            # Create plots
library(reshape2)
library(reshape)


##### Polarity Category #####
p <- ggplot(emo_df, aes(x = pol_best)) +
        geom_bar(aes(y = ..count.., fill = pol_best)) +
        theme(plot.background = element_rect(fill = "darkgrey")) +
        theme(panel.background = element_rect(fill = "grey")) +
        scale_fill_brewer(palette = "Dark2") +
        labs(title = "Classifying Polarity of Stock Headlines",
            x = "Polarity", y = "Count", caption = "Data source: finviz.com")

##### Emotion Category #####
p <- ggplot(emo_df, aes(x = emo_best)) +
         geom_bar(aes(y = ..count.., fill = emo_best)) +
         theme(plot.background = element_rect(fill = "darkgrey")) +
         theme(panel.background = element_rect(fill = "grey")) +
         scale_fill_brewer(palette = "Dark2") +
         labs(title = "Classifying Emotion of Stock Headlines",
             x = "Emotion", y = "Count", caption = "Data source: finviz.com")

# Rename for convenience
colnames(sp500_df)[colnames(sp500_df) == "Tickers"] <- "ticker"
colnames(sp500_df)[colnames(sp500_df) == "GICS.Sector"] <- "sector"

# Merge dataframes on ticker
sec_df <- merge(sp500_df, emo_df, by = intersect(names(sp500_df), names(emo_df)))
sec_df <- as.data.frame(fread("data/50sec_df.tsv", quote = "", header = TRUE))

# Expand pallete from 8 colors to 11
ncolors <- colorRampPalette(brewer.pal(8, "Dark2"))(11)

##### Polarity and Sector #####
p <- ggplot(sec_df, aes(pol_best, ..count..)) +
        geom_bar(aes(fill = sector), position = "dodge") +
        labs(title = "Polarity of Stock Sectors",
            caption = "Data source: finviz.com") +
        theme(plot.background = element_rect(fill = "gray19")) +
        theme(panel.background = element_rect(fill = "gray31")) +
        scale_fill_manual(values = ncolors) +
        theme(axis.text.x = element_text(size = 14, color = "gray70"),
               axis.title.x = element_text(size = 16, color = "gray85"),
               axis.text.y = element_text(size = 14, color = "gray70"),
               axis.title.y = element_text(size = 16, color = "gray85"),
               plot.title = element_text(size = 20, face = "bold",
                   color = "gray85", hjust = 0.5),
               legend.background = element_rect(fill = "gray19"),
               legend.text = element_text(size = 10, color = "gray70"),
               legend.title = element_text(size = 14, color = "gray85"))

##### Mean Polarity Score of Sectors #####
sec <- sec_df %>%
            group_by(sector) %>%
            summarise(pol_score = mean(pol_score)

p <- ggplot(sec, aes(x = sector, y = pol_score, fill = as.factor(sector))) +
        geom_bar(stat = "identity") +
        labs(title = "Mean of Sector's Polarity Score",
                caption = "Data source: finviz.com", fill = 'Sector') +
        theme(plot.background = element_rect(fill = "gray19")) +
        theme(panel.background = element_rect(fill = "gray31")) +
        scale_fill_manual(values = ncolors) +
        theme(axis.text.x = element_text(size = 0),
           axis.title.x = element_text(size = 16, color = "gray85"),
           axis.text.y = element_text(size = 14, color = "gray70"),
           axis.title.y = element_text(size = 16, color = "gray85"),
           plot.title = element_text(size = 20, face = "bold",
               color = "gray85", hjust = 0.5),
           legend.background = element_rect(fill = "gray19"),
           legend.text = element_text(size = 10, color = "gray70"),
           legend.title = element_text(size = 14, color = "gray85"))

# Rename anything containing "consumer" to just "consumer" to reduce number of sectors
df <- copy(sec_df)
for (s in 1:length(df$sector)){
    if (str_detect(df$sector[[s]], "Consumer")){
        df$sector[[s]] <- "Consumer"
    }
}

# Create a column with a 1 if it is after January 6th, 0 if before
df <- df %>%
        mutate(aftjan = if_else(datetime >= as.POSIXct("2021-01-06"), 1, 0))

# Rename the factors if so chooses
# df$aftjan <- factor(df$aftjan, levels = c(0, 1), labels = c("befjan", "aftjan"))

##### Polarity Before and After January 6th Riots #####
p <- ggplot(df) +
        geom_bar(aes(x = aftjan, y = pol_score, fill = as.factor(aftjan)),
                stat = "summary", fun = "mean") +
        labs(title = "Polarity Score Before & After January 6th",
            caption = "Data source: finviz.com") +
        scale_fill_discrete(name = "January 6th", labels = c("Before", "After")) +
        theme(plot.background = element_rect(fill = "gray19")) +
        theme(panel.background = element_rect(fill = "gray31")) +
        theme(axis.text.x = element_text(size = 0),
           axis.title.x = element_text(size = 16, color = "gray85"),
           axis.text.y = element_text(size = 14, color = "gray70"),
           axis.title.y = element_text(size = 16, color = "gray85"),
           plot.title = element_text(size = 20, face = "bold",
               color = "gray85", hjust = 0.5),
           legend.background = element_rect(fill = "gray19"),
           legend.text = element_text(size = 10, color = "gray70"),
           legend.title = element_text(size = 14, color = "gray85"))
# Surprised that the score higher after January 6th than what it is before

##### Distribution of the Polarity Scores of Sectors #####
# Can use coord_flip() to switch instead of manually changing axes
p <- ggplot(sec_df, aes(x = sector, y = pol_score)) +
        geom_boxplot(aes(fill = sector), notch = TRUE, outlier.color = "red3",
            outlier.shape = 8, show.legend = FALSE) +
        coord_flip() +
        scale_fill_manual(values = ncolors) +
        labs(title = "Dist. of the Polarity Scores of Stock Sectors",
            caption = "Data source: finviz.com") +
        stat_summary(fun = mean, geom = "point", shape = 5, size = 4) +
        theme(plot.background = element_rect(fill = "gray19"),
              panel.background = element_rect(fill = "gray31"),
              axis.text.x = element_text(size = 14, color = "gray70"),
              axis.title.x = element_text(size = 16, color = "gray85"),
              axis.text.y = element_text(size = 8, color = "gray70"),
              axis.title.y = element_text(size = 16, color = "gray85"),
              plot.title = element_text(size = 20, face = "bold",
                  color = "gray85", hjust = 0.5))
# slice_max(sec_df, n = 10, order_by = pol_score)       # View highest polarity scores

##### WordCloud #####
# data("stop_words")
tidytext::stop_words
stopwords("en")
library(wordcloud)

plot_wordcloud <- function(topic) {
    filter(str_detect(sec_df, topic)) %>%
        group_by(ticker) %>%
        unnest_tokens(word, sec_df) %>%
        anti_join(stop_words) %>%
        filter(!word %in% topic) %>%
        count(ticker, word, sort = TRUE) %>%
        acast(word ~ ticker, value.var = "n", fill = 0) %>%
        comparison.cloud(colors = c("orange2", "gray20"),
                         max.words = 200, random.order = FALSE)
}

plot_wordcloud("riot")

comparison.cloud()


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
