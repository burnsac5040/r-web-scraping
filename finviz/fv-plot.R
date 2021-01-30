# ================
# title: fv-plot.R
# author: Lucas Burns
# description: Cleaning of fw-scrape.R data and general plotting
# ================

library(rvest)                  # web scraping
library(httr)                   # web scraping
library(dplyr)                  # data wrangling/manipulation
library(stringr)                # string manipulation
library(magrittr)               # piping
library(BatchGetSymbols)        # get stock information
library(flipTime)               # datetime manipulation
library(data.table)             # import/export data
library(tidyverse)              # bunch of data science tools
# install.packages('Rstem', repos = "http://www.omegahat.net/R")
# devtools::install_github("timjurka/sentiment/sentiment")

# ===============
# Getting tickers
# ===============

sp500 <- GetSP500Stocks()
sp500_tickers <- sp500[, c("Tickers", "Company", "GICS.Sector")]
samp_sp500 <- samp_sp500[!grepl("\\.", samp_sp500$Tickers), ]
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

# =================
# Cleaning the data
# =================

# NOTE: Alternative way to read a .tsv file
# pnews_df <- read.table(file = "data/pnews_df.tsv", sep = "\t", header = TRUE)

pnews_df <- as.data.frame(fread("data/50pnews_df-dirty.tsv", quote = "", header = TRUE))
sp500_df <- as.data.frame(fread("data/50sector_df.tsv", quote = ""))

colnames(pnews_df) <- c("ticker", "date", "time", "headline")

# Create a datetime column
pnews_df$datetime_ <- paste(pnews_df$date, pnews_df$time)
pnews_df$datetime <- AsDateTime(pnews_df$datetime_, us.format = TRUE)
pnews_df$datetime_ <- NULL

pnews_df$headline %<>% tolower()

# Deletes 'website.com'
pnews_df$headline <- gsub("[[:blank:]][[:alpha:]]*\\.com$", "", pnews_df$headline)
# pnews_df$headline <- gsub("\\s*\\w*$", "", pnews_df$headline) # Deletes last word

# Delete as many websites without '.com' (there will be some I missed)
pnews_df$headline <- gsub("(investorplace|yahoo finance|motley fool|zacks|reuters|insider monkey|business wire|marketwatch|smarteranalyst|benzinga|globenewswire|techcrunch|quartz|bloomberg|investopedia|newswire|tipranks|gurufocus.com)", "", pnews_df$headline)

# Delete punctuation and digits
pnews_df$headline <- gsub("([[:punct:]]|[[:digit:]])", "", pnews_df$headline)
# Delete random 'pr' scattered throughout
pnews_df$headline <- gsub("\\bpr\\b", "", pnews_df$headline)
# Delete whitespace
pnews_df$headline <- str_squish(pnews_df$headline)

# NOTE: Figuring this out after the fact:
    # Putting this here to show multiple ways of doing things:

# library(tm)
# tm_corpus <- Corpus(VectorSource(pnews_df$headline))
# tmp <- tm_map(tm_corpus, content_transformer(removePunctuation))
# tmp <- tm_map(tm_corpus, content_transformer(removeNumbers))
# tmp <- tm_map(tm_corpus, content_transformer(tolower))
# tmp <- tm_map(tm_corpus, content_transformer(stripWhitespace))
# tmp <- tm_map(tm_corpus, removeWords, stopwords("en"))
# tm_headline <- unlist(as.list(tmp))


#####################################################
################ Polarity / Emotion #################
#####################################################

library(sentiment)      # Rstem, NLP, tm are required
library(sentimentr)
library(syuzhet)
library(tidytext)

# Classify polarity and emotion on entire dataframe
polarity_df <- classify_polarity(pnews_df$headline, algorithm = "bayes")
emotion_df <- classify_emotion(pnews_df$headline, algorithm = "bayes", prior = 1.0)

# Classify sentiment on every word
sentiment_df <- pnews_df %>% unnest_tokens(word, headline) %>% inner_join(get_sentiments("nrc"))

# Returns a one hot encoded dataframe classifying sentiment
nrc <- pnews_df$headline %>% get_sentences() %>% get_nrc_sentiment()


# Read back in the dataframes
pnews_df <- as.data.frame(fread("data/50pnews_df.tsv", quote = "", header = TRUE))
polarity_df <- as.data.frame(fread("data/50polarity_df.tsv", quote = "", header = TRUE))
emotion_df <- as.data.frame(fread("data/50emotion_df.tsv", quote = "", header = TRUE))

# Rename columns for convenience
colnames(polarity_df)[colnames(polarity_df) == "BEST_FIT"] <- "pol_best"
colnames(emotion_df)[colnames(emotion_df) == "BEST_FIT"] <- "emo_best"

# Create a larger combined dataframe
emopol_df <- cbind(pnews_df, polarity_df, emotion_df)
emopol_df <- emopol_df %>% mutate_at('emo_best', ~replace(., is.na(.), 'unknown'))

# Seeing the distribution of positive / negative emotion
pos <- emopol_df[emopol_df$pol_best == 'positive', ]
neg <- emopol_df[emopol_df$pol_best == 'negative', ]
print(glue("pos: {dim(pos)}, neg: {dim(neg)}"))

# Selecting the columns that I am going to use
sel_df <- select(emopol_df, 1, 4, 5, 8:9, 16) %>% as.data.frame()

# Rename column for convenience (should've been done above)
colnames(sel_df)[colnames(sel_df) == "POS/NEG"] <- "pol_score"

# Turning columns into factors
emo_df <- within(sel_df,
    emo_best <- factor(emo_best, levels = names(table(emo_best))),
    pol_best <- factor(pol_best, levels = names(table(pol_best))))

#####################################################
##################### Plotting ######################
#####################################################

emo_df <- as.data.frame(fread("data/50emo_df.tsv", quote = "", header = TRUE))

library(ggthemes)           # theme ggplots
library(RColorBrewer)       # expand number of colors in palette
library(ggplot2)            # Create plots
library(reshape2)           # more efficient reshape

# ==================
# Polarity categoory
# ==================
p <- ggplot(emo_df, aes(x = pol_best)) +
        geom_bar(aes(y = ..count.., fill = pol_best)) +
        theme(plot.background = element_rect(fill = "darkgrey")) +
        theme(panel.background = element_rect(fill = "grey")) +
        scale_fill_brewer(palette = "Dark2") +
        labs(title = "Classifying Polarity of Stock Headlines",
            x = "Polarity", y = "Count", caption = "Data source: finviz.com")

# ================
# Emotion category
# ================
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

# Expand color palette from 8 colors to 11
ncolors <- colorRampPalette(brewer.pal(8, "Dark2"))(11)

# ===================
# Polarity and sector
# ===================
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
               legend.title = element_text(size = 14, color = "gray85")

# ==============================
# Mean polarity score of sectors
# ==============================
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

# Rename anything containing "consumer" to "consumer" to reduce number of sectors
df <- copy(sec_df)
for (s in 1:length(df$sector)){
    if (str_detect(df$sector[[s]], "Consumer")){
        df$sector[[s]] <- "Consumer"
    }
}

# Create a column with a 1 if it is after January 6th, 0 if it is before
df <- df %>%
        mutate(aftjan = if_else(datetime >= as.POSIXct("2021-01-06"), 1, 0))

# Rename the factors if so chooses
# df$aftjan <- factor(df$aftjan, levels = c(0, 1), labels = c("befjan", "aftjan"))

# ===========================================
# Polarity before and after January 6th riots
# ===========================================
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
# Surprised that the score is higher after January 6th than what it is before
# However, this is definitely not the best way to determine this.
# Cleaning the headlines a little more would help

# ==============================================
# Distribution of the polarity scores of sectors
# ==============================================
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
