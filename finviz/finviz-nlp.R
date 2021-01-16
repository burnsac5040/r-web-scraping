# -------------------
# title: finviz-nlp.R
# author: Lucas Burns
# description: Sentiment analysis on finviz.com stock headlines
# -------------------

library(dplyr)
library(stringr)
library(reshape)
library(rvest)
library(httr)
library(XML)
library(RCurl)
library(BatchGetSymbols)
library(flipTime)

# ---------------
# Getting tickers
# ---------------

sp500 <- GetSP500Stocks()
sp500_tickers <- sp500[, c("Tickers", "Company", "GICS.Sector")]
samp_tick <- slice_sample(sp500_tickers, n=250)$Tickers
samp_tick <- samp_tick[grep("\\.", samp_tick, invert = TRUE)]


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
    }
    return(all_dfs)
}

parsed_dfs <- get_newstable(samp_tick)
pnews_df <- do.call(rbind, parsed_dfs)
