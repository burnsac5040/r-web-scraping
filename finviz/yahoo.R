# ------------
# title: yahoo.R
# author: Lucas Burns
# description: Scraping and cleaning Yahoo stock data
# ------------

library(dplyr)
library(stringr)
library(rvest)
library(httr)
library(XML)
library(RCurl)

# ----------
# One page #
# ----------

ya_url <- "https://finance.yahoo.com/quote/GOOG"

parsed <- htmlParse(GET(ya_url), encoding="UTF-8")
wtables <- readHTMLTable(parsed)
wtable <- rbind(wtables[[1]][2], wtables[[2]][2]) # Combine only the numbers
names(wtable) <- "GOOG" # Rename column

namesrows <- rbind(wtables[[1]][1], wtables[[2]][1]) # Combine only rownames
row.names(wtable) <- namesrows[,1] # Rename the number df"s rows

# 52 Week Range
wtable["year_low", ] <- str_split_fixed(wtable["52 Week Range", ], " - ", 2)[1]
wtable["year_low", ] <- wtable["year_low", ] %>% gsub(",", "", .) %>% as.numeric()
wtable["year_high", ] <- str_split_fixed(wtable["52 Week Range", ], " - ", 2)[2]
wtable["year_high", ] <- wtable["year_high", ] %>% gsub(",", "", .) %>% as.numeric()

# Day"s Range
wtable["day_low", ] <- str_split_fixed(wtable["Day's Range", ], " - ", 2)[1]
wtable["day_low", ] <- wtable["day_low", ] %>% gsub(",", "", .) %>% as.numeric()
wtable["day_high", ] <- str_split_fixed(wtable["Day's Range", ], " - ", 2)[2]
wtable["day_high", ] <- wtable["day_high", ] %>% gsub(",", "", .) %>% as.numeric()
# Dropping the modified rows
wtable <- wtable[!rownames(wtable) %in% c("52 Week Range", "Day's Range"), , drop=FALSE]


# ---------------
# Renaming rows #
# ---------------
# NOTE: I"m renaming the rows after scraping the table instead of setting them when scraping just in case there is a stock table I scrape that has +1/-1 row resulting in the row names being wrong and me not knowing that it is.

# Previous Close
rownames(wtable)[rownames(wtable) == "Previous Close"] <- "prev_close"
wtable["prev_close", ] <- wtable["prev_close", ] %>% gsub(",", "", .) %>% as.numeric()

# Open
rownames(wtable)[rownames(wtable) == "Open"] <- "open"
wtable["open", ] <- wtable["open", ] %>% gsub(",", "", .) %>% as.numeric()

# Bid
wtable["bid_price", ] <- str_split_fixed(wtable["Bid", ], " x ", 2)[1] %>% gsub(",", "", .) %>% as.numeric()
wtable["bid_size", ] <- str_split_fixed(wtable["Bid", ], " x ", 2)[2] %>% gsub(",", "", .) %>% as.numeric()
wtable <- wtable[rownames(wtable) != "Bid", , drop=FALSE]

# Ask
wtable["ask_price", ] <- str_split_fixed(wtable["Ask", ], " x ", 2)[1] %>% gsub(",", "", .) %>% as.numeric()
wtable["ask_size", ] <- str_split_fixed(wtable["Ask", ], " x ", 2)[2] %>% gsub(",", "", .) %>% as.numeric()
wtable <- wtable[rownames(wtable) != "Ask", , drop=FALSE]

# Volume
rownames(wtable)[rownames(wtable) == "Volume"] <- "volume"
wtable["volume", ] <- wtable["volume", ] %>% gsub(",", "", .) %>% as.numeric()

# Avg. Volume
rownames(wtable)[rownames(wtable) == "Avg. Volume"] <- "avg_volume"
wtable["avg_volume", ] <- wtable["avg_volume",] %>% gsub(",", "", .) %>% as.numeric()

# Market Cap
mc_map <- list("M"=1000000, "B"=1000000000, "T"=1000000000000)
mc_mapped <- c()

for (el in names(mc_map)){
    for (mc in wtable["Market Cap", ]){
        if (grepl(el, mc, fixed=TRUE)){
            dig <- str_extract(wtable["Market Cap", ], "\\d+.\\d+") %>% as.numeric()
            print(dig)
            mc <- as.numeric(gsub(mc, mc_map[[el]], mc))
            print(mc)
            mc_mapped <- append(mc_mapped, (dig * mc))
        }
    }
}

wtable['market_cap', ] <- mc_mapped

# Beta (5Y Monthly)
rownames(wtable)[rownames(wtable) == "Beta (5Y Monthly)"] <- "beta_5y"
wtable["beta_5y", ] <- wtable["beta_5y", ] %>% as.numeric()

# PE Ratio
rownames(wtable)[rownames(wtable) == "PE Ratio (TTM)"] <- "pe_ratio_ttm"
wtable["pe_ratio_ttm", ] <- wtable["pe_ratio_ttm", ] %>% gsub(",", "", .) %>% as.numeric()

# EPS
rownames(wtable)[rownames(wtable) == "EPS (TTM)"] <- "eps_ttm"
wtable["eps_ttm", ] <- wtable["eps_ttm", ] %>% gsub(",", "", .) %>% as.numeric()

# 1y Target Est
rownames(wtable)[rownames(wtable) == "1y Target Est"] <- "1y_targ_est"
wtable["1y_targ_est", ] <- wtable["1y_targ_est", ] %>% gsub(",", "", .) %>% as.numeric()

# Earnings Rate
rownames(wtable)[rownames(wtable) == "Earnings Date"] <- "earnings_date"
wtable["earnings_date", ] <- wtable["earnings_date", ] %>% as.numeric()

# Forward Dividend & Yield
wtable['forward_div', ] <- str_split_fixed(wtable['Forward Dividend & Yield', ], ' ', 2)[[1]] %>% as.numeric()
wtable['yield', ] <- str_split_fixed(wtable['Forward Dividend & Yield', ], ' ', 2)[[2]] %>% as.numeric()
wtable <- wtable[rownames(wtable) != "Forward Dividend & Yield", , drop=FALSE]

# Ex-Dividend Date
rownames(wtable)[rownames(wtable) == "Ex-Dividend Date"] <- "ex_div_date"
wtable['ex_div_date', ] <- wtable['ex_div_date', ] %>% as.numeric()
