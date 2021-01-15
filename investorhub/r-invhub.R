library(dplyr)
library(stringr)
library(flipTime)
library(httr)
library(rvest)
library(XML)
library(RCurl)

#-----------
# One Page #
#-----------
base_url <- "https://investorshub.advfn.com/boards/recentnews.aspx?page=11"

parsed <- htmlParse(GET(base_url), encoding="UTF-8")
wtable <- readHTMLTable(parsed)
wtable <- wtable[[1]]
names(wtable) <- c('symb', 'quote', 'change', 'pcnt', 'headline', 'time')
wtable$change <- as.numeric(wtable$change)
wtable$pcnt <- wtable$pcnt %>% gsub("%", "", .) %>% as.numeric()
wtable$time <- AsDateTime(wtable$time)


# ---------------
# Mutiple Pages #
# ---------------
base_url <- "http://investorshub.advfn.com/boards/recentnews.aspx?page="

hub_df <- data.frame()
i = 1

while (i < 30){
  parsed <- htmlParse(GET(paste0(base_url, i), encoding="UTF-8"))
  wtable <- readHTMLTable(parsed)
  wtable <- wtable[[1]]

  namecols <- c('symb', 'quote', 'change', 'pcnt', 'headline', 'time')
  names(wtable) <- namecols

  wtable$quote <- wtable$quote %>% gsub(",", "", .) %>% as.numeric()
  wtable$change <- as.numeric(wtable$change)
  wtable$pcnt <- wtable$pcnt %>% gsub("%", "", .) %>% as.numeric()
  wtable$time <- AsDateTime(wtable$time)

  hub_df <- rbind(hub_df, wtable)
  colnames(hub_df) <- namecols

  i = i + 1
  Sys.sleep(3)
}

# NOTE: Remove N/A's
hub_df <- hub_df[complete.cases(hub_df), ] # removing NA's
colSums(is.na(hub_df)) # check to make sure there are no NA's

write.csv(hub_df, file='hub_df.csv', row.names=FALSE)
write.table(hub_df, file='hub_df.tsv', sep="\t", row.names=FALSE,
			col.names=FALSE, quote=FALSE)


