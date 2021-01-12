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

base_url<- "https://news.ycombinator.com/front"
page <- read_html(base_url)

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

# Generate list of URLs to use
urls <- c()
for (i in 1:12){
	for (j in 1:4){
		turl <- glue(base_url, "?day=2021-01-0{i}&p={j}")
		urls <- append(urls, turl)
	}
}

urls

# testing to make sure they work
thours <- get_hours(get_tda(read_html(urls[[1]])))

# ------------------
# Post Title Block #
# A way to track NA
# ------------------

library(zeallot) # uses %<-% to return multiple values

get_ptitleblk <- function(page){
"Returns post title, external link, and post rank"
	ptitleblk <- html_nodes(page, '.title') %>%
		html_text() %>%
		grep('more', ., ignore.case=TRUE, invert=TRUE, value=TRUE)

	# Rank
	prank <- ptitleblk[c(TRUE, FALSE)] %>%
		gsub("\\.", "", .) %>%
		as.numeric()

	titnsite <- ptitleblk[c(FALSE, TRUE)] %>%
		str_split('\\(', simplify=FALSE)

	# Title
	ptitle <- lapply(titnsite, "[", 1) %>%
		trimws()

	# External Site
	psite <- c()
	for (i in titnsite){
		s <- rev(i)[1] %>% gsub("\\/.*|\\)", "", .)
		psite <- append(psite, s)
}
	return(list(prank, ptitle, psite))
}

c(prank, ptitle, psite) %<-%  get_ptitleblk(page)


# --------------
# Post Subtext #
# --------------

get_subtext <- function(page){
"Returns post score, user, time, and number of comments"
	psubtext <- html_nodes(page, '.subtext') %>%
		html_text() %>%
		gsub("\\n", "", .) %>%
		trimws()

	# Score
	pscore <- gsub('points.*', "\\1", psubtext) %>%
		trimws() %>%
		as.numeric()

	# User
	puser <- gsub('.*by', '', psubtext) %>%
		gsub('\\d.*', '', .) %>%
		trimws()

	# Hours ago
	phours <- gsub('ago.*', '', psubtext) %>%
		str_split(" ") %>%
		lapply('[', 5:6)
	phours <- paste(lapply(phours, '[', 1), lapply(phours, '[', 2))
	conv <- list('hour'=1, 'day'=24)
	hrs <- c()

	for (v in names(conv)){
		for (d in phours){
			if (grepl(v, d, fixed=TRUE)){
				n <- as.numeric(gsub("([0-9]+).*$", "\\1", d))
				d <- as.numeric(gsub(d, conv[[v]], d))
				hrs <- append(hrs, (d*n))
			}
		}
	}

	# Comments
	pcomments <- gsub('.*\\shide\\s\\|', '', psubtext) %>%
		gsub('comment(s)?', '', .) %>%
		trimws() %>%
		as.numeric()

	return(list(pscore, puser, hrs, pcomments))
}


c(pscore, puser, phours, pcomments) %<-% get_subtext(page)

# -----------
# Dataframe #
# -----------

tdf <- tibble(title=ptitle, rank=prank, site=psite,
		score=pscore, user=puser, hours=phours, n_comm=pcomments)


write.csv(tdf, file='tdf.csv', row.names=FALSE)
write.table(tdf, file='tdf.tsv', sep="\t", row.names=FALSE,
			col.names=FALSE, quote=FALSE)


# ----------------------------
# Scraping data on all pages #
# ----------------------------

# Testing to make sure the loop works
y <- urls[1:2]

tt <- tibble(title=NA, rank=NA, site=NA, score=NA, user=NA, hours=NA, n_comm=NA)
for (i in y){
	page <- read_html(i)
	c(prank, ptitle, psite) %<-%  get_ptitleblk(page)
	c(pscore, puser, phours, pcomments) %<-% get_subtext(page)

	tdf <- tibble(title=ptitle, rank=prank, site=psite,
		score=pscore, user=puser, hours=phours, n_comm=pcomments)

	tt <- bind_rows(tt, tdf)
}

tt[-c(1),]


# Performing the loop
tt <- tibble(title=NA, rank=NA, site=NA, score=NA, user=NA, hours=NA, n_comm=NA)
dfs <- list()

for (url in urls){
	page <- read_html(url)
	c(prank, ptitle, psite) %<-%  get_ptitleblk(page)
	c(pscore, puser, phours, pcomments) %<-% get_subtext(page)

	tdf <- data.frame(title=ptitle, rank=prank, site=psite,
		score=pscore, user=puser, hours=phours, n_comm=pcomments)

	dfs <- append(dfs, tdf)
}

library(data.table)
library(plyr)
# ldply(dfs, data.frame)
# bind_rows(dfs, .id="column_label")
# do.call("rbind", dfs)


# rbindlist(dfs)

# matrix(c(ptitle, puser), byrow=FALSE)
