library(rvest) # Web scraping library
library(comprehenr)
library(tidyverse)
library(GetoptLong) # library for string substitution
# library(gsubfn)
# library(stringr)
# library(sjmisc)
# library(dict)
# library(itertools)

# List Comprehension Examples
# to_list(for(i in 1:10) if(i %% 2==0) i*i)
# to_list(for(i in 1:length(test)) if(i %% 5==0) test[i])
# test[seq(1, length(test), 5)]
# test[c(rep(FALSE, 5), TRUE)]

# ==========
### One Page
# ==========

url <- "https://news.ycombinator.com/front"
page <- read_html(url)

# ----------
# Post title
# ----------
get_ptitle <- function(page){
	ptitle <- html_nodes(page, '.storylink') %>% html_text()
}

ptitle <- get_ptitle(page)

# -------
# Upvotes
# -------
get_score <- function(page){
	score <- html_nodes(page, '.score') %>%
		html_text() %>%
		gsub(" points", "", .) %>%
		as.numeric()
}

score <- get_score(page)
enumerate()
# -------------------------
# Website that is referenced
# MISSING VALUES, CHANGE FIGURE OUT ZIPPING TOGETHER ####
# -------------------------
get_site <- function(page){
	sitestr <- html_nodes(page, '.sitestr') %>%
		html_text() %>%
		gsub("/.*", "", .) # strip everything after / to get base url

	sitestr <- append(sitestr, 'N/A', 13-1)
}

sitestr <- get_site(page)
html_nodes(page, '.sitestr')

# library(magrittr)

# to_vec(for(`i, j` in zip_lists(html_nodes(page, '.sitestr'), html_nodes(page, '.rank')))i, j)


# --------
# Username
# --------
get_user <- function(page){
	user <- html_nodes(page, '.hnuser') %>%
		html_text()
}

user <- get_user(page)

# ------------------
# Table Data to Parse
# ------------------
get_tda <- function(page){
	tda <- html_nodes(page, 'td a') %>% html_text()
}

tda <- get_tda(page)

# ---------------------------
# Time (ago) the post was made
# ---------------------------
get_hours <- function(tda){
	daysago <- to_vec(for(i in tda) if(grepl('ago', i, fixed=TRUE)) i) %>%
				gsub("\\sago", "", .)

	conv <- list('hour'=1, 'day'=24)
	hrs <- list()

	for (v in names(conv)){
		for (d in daysago){
			if (grepl(v, d, fixed=TRUE)){
				n <- as.numeric(gsub("([0-9]+).*$", "\\1", d))
				d <- as.numeric(gsub(d, conv[[v]], d))
				hrs <- append(hrs, (d*n))
			}
		}
	}
	return(unlist(hrs))
}

hrs <- get_hours(tda)

# ------------
# Post Comments
# ------------
get_pcomments <- function(tda){
	pcomments <- to_vec(for(i in tda) if(grepl("\\d+\\scomments", i))  i)  %>%
				gsub("\\scomments", "", .) %>%
				as.numeric()
}

pcomments <- get_pcomments(tda)

# ---------
# Dataframe
# ---------
df <- data.frame(post_title=c(ptitle), score=c(score),
				 site=c(sitestr), user=c(user),
				 hours_ago=hrs, num_comments=c(pcomments))

write.csv(df, file='df.csv')

# ===========
### All Pages
# ===========

# -----------
# List of URLs
# ------------
morelink <- function(page){
	html_nodes(page, '.morelink') %>%
		html_attr('href')

}

href <- morelink(page)

library(glue)

urls <- list()
for (i in 1:6){
	for (j in 1:4){
		turl <- glue(url, "front?day=2021-01-0{i}&p={j}")
		urls <- append(urls, turl)
	}
}




