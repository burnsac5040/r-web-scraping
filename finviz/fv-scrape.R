# -------------
# title: finviz.R
# author: Lucas Burns
# There may be 'NOTE:' scattered throughout because I am in the process of learning R and Vim highlights that word.
# -------------
library(dplyr)
library(stringr)
library(reshape)
library(rvest)
library(httr)
library(XML)
library(RCurl)

##############################################
################## One Page ##################
##############################################

#### Chart ####

base_url <- "https://finviz.com/quote.ashx?t="
tickers <- c("HD")

page <- htmlParse(GET(paste0(base_url, tickers[[1]])), encoding = "UTF-8")
tables <- readHTMLTable(page)
wtable <- tables[[8]]

t2 <- wtable %>% select(1, 2)
t4 <- wtable %>% select(3, 4)
t6 <- wtable %>% select(5, 6)
t8 <- wtable %>% select(7, 8)
t10 <- wtable %>% select(9, 10)
t12 <- wtable %>% select(11, 12)

t2 <- setNames(rbind(names(t2), t2), c("Spec", "Value"))
t4 <- setNames(rbind(names(t4), t4), c("Spec", "Value"))
t6 <- setNames(rbind(names(t6), t6), c("Spec", "Value"))
t8 <- setNames(rbind(names(t8), t8), c("Spec", "Value"))
t10 <- setNames(rbind(names(t10), t10), c("Spec", "Value"))
t12 <- setNames(rbind(names(t12), t12), c("Spec", "Value"))

tot <- rbind(t2, t4, t6, t8, t10, t12)

# NOTE: Drop duplicates
# unique(tot[, 1:2])
# tot[!duplicated(tot), ]
tot <- distinct(tot, Spec, .keep_all=TRUE)
rownames(tot) <- tot$Spec

# NOTE: Drop a column
tot <- within(tot, rm(Spec))
# tot <- subset(tot, select=-c(Spec))
# tot$Spec <- NULL # Another way to remove the column

tot <- as.data.frame(t(as.matrix(tot))) # Transpose dataframe
rownames(tot) <- tickers[[1]]

#### News Articles ####
page <- read_html(paste0(base_url, tickers[[1]]))
# newstable <- html_nodes(page, "table")[[32]] %>% html_table(fill=TRUE)
# NOTE: Better way to scrape, can use id/class (similar to BeautifulSoup)
newstable <- html_nodes(page, "[id='news-table']") %>% html_table(fill=TRUE)
news_df <- data.frame(newstable)

# library(flipTime)
# AsDateTime(news_df[,1])

dfs <- list()

for (ticker in tickers){
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
}

parsed_df <- do.call(rbind, dfs)

# NOTE: Alternative way to iterate over two variables at the same time
# library(foreach)
# foreach(dt = news_df[, 1], headl = news_df[, 2]) %do% {
#     print(dt)
# }


##############################################
############### Multiple Pages ###############
##############################################

# Categories -- Some of these may not be split up exactly
# ----------

financial <- list("Paypal" = "PYPL", "Visa" = "V", "Mastercard" = "MA",
               "Discover" = "DFS", "CitiGroup" = "C", "Square" = "SQ",
               "GoldmanSachs" = "GS", "AmerExp" = "AXP", "ProgLease" = "PRG")

bigtech <- list("Google" = "GOOGL", "Facebook" = "FB", "Twitter" = "TWTR",
             "Snapchat" = "SNAP", "Apple" = "AAPL", "Microsoft" = "MSFT",
             "Netflix" = "NFLX", "Tesla" = "TSLA", "Nvidia" = "NVDA",
             "Intel" = "INTC")

consumer <- list("Alibaba" = "BABA", "Amazon" = "AMZN", "Ebay" = "EBAY",
              "WalMart" = "WMT", "Target" = "TGT", "Costco" = "COST",
              "DollarGen" = "DG", "HomeDepot" = "HD")

industrial <- list("3M" = "MMM", "GenElect" = "GE", "KCSouth" = "KSU",
                "LockHeed" = "LMT", "Ratheon" = "RTX", "Honeywell" = "HON",
                "Boeing" = "BA", "Rockwell" = "ROK")

fastfood <- list("McDonalds" = "MCD", "Wendys" = "WEN", "JackInBox" = "JACK",
              "Dominos" = "DPZ", "Chipotle" = "CMG", "RestBrandInt" = "QSR")

# ----------------------
# Getting all the tables
# ----------------------
# NOTE: Alternative way to skip errors:
    # skip_to_next <- FALSE
    # tryCatch(tables[[8]], error = function(e) {skip_to_next <<- TRUE})
    # if (skip_to_next) {next}

get_specs <- function(tickers){
    parsed_specs <- data.frame()

    for (ticker in tickers){
        page <- htmlParse(GET(paste0(base_url, ticker[[1]]), encoding = "UTF-8"))
        tables <- readHTMLTable(page)

        possibleError <- tryCatch(
                wtable <- tables[[8]],
                error = function(e) e
                )

        if (inherits(possibleError, "error")) next

        t2 <- wtable %>% select(1, 2)
        t4 <- wtable %>% select(3, 4)
        t6 <- wtable %>% select(5, 6)
        t8 <- wtable %>% select(7, 8)
        t10 <- wtable %>% select(9, 10)
        t12 <- wtable %>% select(11, 12)

        t2 <- setNames(rbind(names(t2), t2), c("Spec", "Value"))
        t4 <- setNames(rbind(names(t4), t4), c("Spec", "Value"))
        t6 <- setNames(rbind(names(t6), t6), c("Spec", "Value"))
        t8 <- setNames(rbind(names(t8), t8), c("Spec", "Value"))
        t10 <- setNames(rbind(names(t10), t10), c("Spec", "Value"))
        t12 <- setNames(rbind(names(t12), t12), c("Spec", "Value"))

        tot <- rbind(t2, t4, t6, t8, t10, t12)

        tot <- distinct(tot, Spec, .keep_all = TRUE)
        rownames(tot) <- tot$Spec
        tot <- tot[, -c(1), drop=FALSE]
        tot <- as.data.frame(t(as.matrix(tot)))
        rownames(tot) <- ticker[[1]] # Not visible in visidata
        tot <- cbind("ticker"=ticker, tot)

        parsed_specs <- rbind(parsed_specs, tot)
    }
    return(parsed_specs)
}

# get_specs(unname(c(financial, bigtech, consumer, industrial, fastfood)))

financial_df <- get_specs(unname(financial))
bigtech_df <- get_specs(unname(bigtech))
consumer_df <- get_specs(unname(consumer))
industrial_df <- get_specs(unname(industrial))
ff_df <- get_specs(unname(fastfood))

tot_df <- rbind(financial_df, bigtech_df, consumer_df, industrial_df, ff_df)

# -----------------------
# Price Data: `fastfood`
# -----------------------
library(BatchGetSymbols)

date1 <- Sys.Date() - 60
date2 <- Sys.Date()

bgs <- BatchGetSymbols(tickers = unname(fastfood),
                first.date = date1,
                last.date = date2,
                freq.data = 'daily',
                cache.folder = file.path(tempdir(),
                                         "BGS_Cache"))

bgs_tickers <- bgs$df.tickers

# --------------------
# S&P500: Stock Tickers
# --------------------
sp500 <- GetSP500Stocks()
sp500_tickers <- sp500[, c("Tickers", "Company", "GICS.Sector")]

samp_tick <- slice_sample(sp500_tickers, n=250)$Tickers

# NOTE: Indexing a dataframe (similar to Python)
# sp500[sp500$Tickers == "GOOG", ]
# sp500_df[order(sp500_df$ticker),]

sp500_df <- get_specs(samp_tick)

# ------------------
# S&P500: Stock Data
# ------------------
sp500_out <- BatchGetSymbols(tickers = sp500$Tickers,
                first.date = date1,
                last.date = date2,
                freq.data = 'daily',
                cache.folder = file.path(tempdir(),
                                         "BGS_Cache"))
sp500_out$df.tickers$ref.date
