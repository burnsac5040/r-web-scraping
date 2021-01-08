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
library(GetoptLong)
# to_list(for(i in 1:10) if(i %% 2==0) i*i)
# to_list(for(i in 1:length(test)) if(i %% 5==0) test[i])
# test[seq(1, length(test), 5)]
# test[c(rep(FALSE, 5), TRUE)]

daysago <- to_vec(for(i in tda) if(grepl('ago', i, fixed=TRUE)) i) %>%
				gsub("\\sago", "", .)


l <- list('hour'=1, 'day'=24)
hrs <- list()

for (v in names(l)){
	for (d in daysago){
		if (grepl(v, d, fixed=TRUE)){
			n <- as.numeric(gsub("([0-9]+).*$", "\\1", d))
			d <- as.numeric(gsub(d, l[[v]], d))
			hrs <- append(hrs, (d*n))
		}
	}
}


pcomments <- to_vec(for(i in tda) if(grepl("\\d+\\scomments", i))  i)  %>%
				gsub("\\scomments", "", .) %>%
				as.numeric()

df <- data.frame(post_title=c(ptitle), score=c(score), 
				 site=c(sitestr), user=c(user), 
				 hours_ago=c(hrs), num_comments=c(pcomments))

write.csv(df, file='df.csv')
