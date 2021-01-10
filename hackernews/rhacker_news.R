library(rvest) # Web scraping library
library(comprehenr) # list comprehension library
library(tidyverse)
library(GetoptLong) # library for string substitution
# library(gsubfn)
library(stringr)
library(rebus)

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
	ptitle <- html_nodes(page, '.storylink') %>%
		html_text() %>%
		trimws()
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

# -------------------------
# Website that is referenced
#### MISSING VALUES, CHANGE FIGURE OUT ZIPPING TOGETHER ####
# -------------------------
get_site <- function(page){
	sitestr <- html_nodes(page, '.sitestr') %>%
		html_text() %>%
		gsub("/.*", "", .) %>% # get base url
		trimws()
}

sitestr <- get_site(page)

# to_vec(for(`i, j` in zip_lists(html_nodes(page, '.sitestr'), html_nodes(page, '.rank')))i, j)

# --------
# Username
# --------
get_user <- function(page){
	user <- html_nodes(page, '.hnuser') %>%
		html_text() %>%
		trimws()
}

user <- get_user(page)

# ------------------
# Table Data to Parse
# ------------------
get_tda <- function(page){
	tda <- html_nodes(page, 'td a') %>%
		html_text() %>%
		trimws()
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
	ncomments <- to_vec(for(i in tda) if(grepl("\\d+\\scomments", i))  i)  %>%
				gsub("\\scomments", "", .) %>%
				as.numeric()
}

ncomments <- get_pcomments(tda)

# ---------
# dataframe
# ---------
# df <- data.frame(post_title=c(ptitle), score=c(score),
# 				 site=c(sitestr), user=c(user),
# 				 hours_ago=hrs, num_comments=c(ncomments))

# write.csv(df, file='df.csv', row.names=FALSE)

# # Working with tibble
# tdf <- tibble(post_title=ptitle, score=score, site=sitestr,
# 			user=user, hours=hrs, n_comm=ncomments)


# write.csv(tdf, file='tdf.csv', row.names=FALSE)
# write.table(tdf, file='tdf.tsv', sep="\t", row.names=FALSE,
# 			col.names=FALSE, quote=FALSE)

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
for (i in 1:9){
	for (j in 1:4){
		turl <- glue(url, "?day=2021-01-0{i}&p={j}")
		urls <- append(urls, turl)
	}
}

# testing to make sure they work
thours <- get_hours(get_tda(read_html(urls[[1]])))

# ----------------
# Post title block (zipping with rank to be able to fill na)
# Implements a much better way of scraping data than above
# ----------------

library(zeallot) # uses %<-% to return multiple values

"Returns post title, external link, and post rank"
get_ptitleblk <- function(page){
	ptitleblk <- html_nodes(page, '.title') %>%
		html_text() %>%
		grep('more', ., ignore.case=TRUE, invert=TRUE, value=TRUE)

	prank <- ptitleblk[c(TRUE, FALSE)] %>%
		gsub("\\.", "", .) %>%
		as.numeric()

	titnsite <- ptitleblk[c(FALSE, TRUE)] %>%
		str_split('\\(', simplify=FALSE)

	ptitle <- lapply(titnsite, "[", 1) %>%
		trimws()

	psite <- lapply(titnsite, "[", 2) %>%
		gsub("/.*|\\)", "", .) %>%
		trimws()

	return(list(prank, ptitle, psite))
}

c(prank, ptitle, psite) %<-%  get_ptitleblk(page)

# df <- data.frame(rank=prank, post_title=ptitle), score=c(score),
# 				 site=c(sitestr), user=c(user),
# 				 hours_ago=hrs, num_comments=c(ncomments))

# Working with tibble
tdf <- tibble(rank=prank, post_title=ptitle, score=score, site=psite,
			user=user, hours=hrs, n_comm=ncomments)

tdf
write.csv(tdf, file='tdf.csv', row.names=FALSE)
write.table(tdf, file='tdf.tsv', sep="\t", row.names=FALSE,
			col.names=FALSE, quote=FALSE)


library(xts)
library(infer)
