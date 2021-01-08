# https://cran.r-project.org/web/packages/comprehenr/vignettes/Introduction.html
library(rvest)

# One Page
url <- "https://news.ycombinator.com/front"
page <- read_html(url)

ptitle <- html_nodes(page, '.storylink') %>% html_text()
score <- html_nodes(page, '.score') %>%
	html_text() %>%
	gsub(" points", "", .) %>%
	as.numeric()

sitestr <- html_nodes(page, '.sitestr') %>%
	html_text() %>%
	gsub("/.*", "", .) # strip everything after / to get base url
sitestr <- append(sitestr, 'N/A', 13-1)

user <- html_nodes(page, '.hnuser') %>%
	html_text()

tda <- html_nodes(page, 'td a') %>% html_text()

library(comprehenr)
library(tidyverse)
# library(gsubfn)
# library(stringr)
# library(sjmisc)

library(dict)
# https://www.rdocumentation.org/packages/Dict/versions/0.10.0
# devtools::install_github('mkuhn/dict')

# to_list(for(i in 1:10) if(i %% 2==0) i*i)
# to_list(for(i in 1:length(test)) if(i %% 5==0) test[i])
# test[seq(1, length(test), 5)]
# test[c(rep(FALSE, 5), TRUE)]

d <- dict()
d[['hours']] <- 1
d[['hour']] <- 1
d[['days']]  <- 24
d[['day']] <- 24
daysago[1]

daysago <- to_vec(for(i in tda) if(grepl('ago', i, fixed=TRUE)) i) %>%
				gsub("\\sago", "", .)

pcomments <- to_vec(for(i in tda) if(grepl("\\d+\\scomments", i))  i)  %>%
				gsub("\\scomments", "", .) %>%
				as.numeric()

df <- data.frame(post_title=c(ptitle), score=c(score), 
				 site=c(sitestr), user=c(user), 
				 days_ago=c(daysago), num_comments=c(pcomments))

write.csv(df, file='df.csv')
