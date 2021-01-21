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
library(ggplot2)
library(tidyquant)
library(glue)
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

        tryCatch({
            page <- read_html(paste0(base_url, ticker[[1]]))
            newstable <- html_nodes(page, "[id='news-table']") %>%
                                                html_table(fill = TRUE)
        },
        error = function(e) {skip <<- TRUE}
        )

        if (skip) {next}

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
        print(glue("Completed ticker: {ticker}"))
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

# Separate dataframes of before the January 6th riot and after
befjan <- news_df[news_df$datetime < as.POSIXct("2021-01-06"), ]
aftjan <- news_df[news_df$datetime >= as.POSIXct("2021-01-06"), ]

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
#################### TESTING NLP ####################
#####################################################

library(Rstem)
library(sentiment)

library(NLP)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(sentimentr)
library(tidytext)

# Classify polarity and emotion
polarity_df <- classify_polarity(pnews_df$headline, algorithm = "bayes")
emotion_df <- classify_emotion(pnews_df$headline, algorithm = "bayes", prior = 1.0)

# Read back in the dataframes
pnews_df <- as.data.frame(fread("data/50pnews_df.tsv", quote = "", header = TRUE))
polarity_df <- as.data.frame(fread("data/50polarity_df.tsv", quote = "", header = TRUE))
emotion_df <- as.data.frame(fread("data/50emotion_df.tsv", quote = "", header = TRUE))

names(polarity_df)[names(polarity_df) == "BEST_FIT"] <- "pol_best"
names(emotion_df)[names(emotion_df) == "BEST_FIT"] <- "emo_best"

# Create a larger combined dataframe
emopol_df <- cbind(pnews_df, polarity_df, emotion_df)
emopol_df

pos <- emopol_df[emopol_df$pol_best == 'positive', ]
neg <- emopol_df[emopol_df$pol_best == 'negative', ]

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

stem <- function(list){
  for (i in c(1:dim(pos)[[1]])){
    list[i,1] <- wordStem(String(list[i,1]))
  }
  list <- unique(list[,1])
}


ww <- get_wordlist(st)
ww

for (i in c(1:length(ww))){

}





positive <- stem(ww)
positive
