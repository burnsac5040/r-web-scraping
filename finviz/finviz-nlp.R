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
pnews_df
# pnews_df <- pnews_df[,-1]
# pnews_df <- pnews_df[, !(names(pnews_df) %in% c("pdatetime"))]

pnews_df$datetime_ <- paste(pnews_df$date, pnews_df$time)
pnews_df$datetime <- AsDateTime(pnews_df$datetime_, us.format = TRUE)
pnews_df$datetime_ <- NULL

pnews_df$headline %<>% tolower()
pnews_df$headline <- gsub("(yahoo finance|barrons.com|motley fool|zacks|reuters|insider monkey|business wire|marketwatch|smarteranalyst|benzinga|globenewswire|techcrunch|quartz|bloomberg|investopedia|newswire|tipranks)", "", pnews_df$headline)
pnews_df$headline <- gsub("([[:punct:]]|[[:digit:]])", "", pnews_df$headline)
pnews_df$headline <- str_squish(pnews_df$headline)

####################################################
###################### NLP #########################
####################################################
news_df <- copy(pnews_df)

befjan <- news_df[news_df$datetime < as.POSIXct("2021-01-06"), ]
aftjan <- news_df[news_df$datetime >= as.POSIXct("2021-01-06"), ]

riot <- aftjan[aftjan$headline %like% "riot", ]
capitol <- aftjan[aftjan$headline %like% "capitol", ]
qanon <- aftjan[aftjan$headline %like% "qanon", ]
war <- aftjan[aftjan$headline %like% "war", ]

library(NLP)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(sentimentr)
library(tidytext)

test <- slice_sample(news_df, n=20)

riot <- news_df %>% filter(str_detect(news_df$headline, "riot"))
riot

write.table(pnews_df, file="data/pnews_df.tsv", quote = FALSE, sep="\t", row.names = FALSE)
write.csv(pnews_df, file = "data/pnews_df.csv")

#####################################################
#################### TESTING NLP ####################
#####################################################

library(Rstem)
library(sentiment)

polarity_df <- classify_polarity(news_df$headline, algorithm = "bayes")
polarity_df <- as.data.frame(fread("data/50polarity_df.tsv", quote = "", header = TRUE))

polarity_df <- cbind(pnews_df, polarity_df)
pos <- polarity_df[polarity_df$BEST_FIT == 'positive', ]
neg <- polarity_df[polarity_df$BEST_FIT == 'negative', ]

for (i in c(1:nrow(pos$headline))){
  pos$headline[i, 1] <- wordStem(String(pos$headline[i, 1]))
}

stem <- function(list){
  for (i in c(1:dim(pos)[[1]])){
    list[i,1] <- wordStem(String(list[i,1]))
  }
  list <- unique(list[,1])
}




for (i in c(1:dim(pos)[[1]])){
  print(i)
}

r <- unique
