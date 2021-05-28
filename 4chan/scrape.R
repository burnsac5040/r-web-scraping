############################################################################
#    Author: Lucas Burns                                                   #
#     Email: burnsac@me.com                                                #
#     Title: scrape.R                                                      #
#   Created: 2021-05-26 11:32                                              #
############################################################################

# == imports == {{{
library(dplyr)
library(stringr)
library(magrittr)
library(httr)
library(rvest)
library(XML)
library(purrr)
# }}} == imports ==

# help {{{
ls("package:magrittr")
help(unique)
# trim whitespace
trimws()
str_trim() # stringr
# }}} help

#############################################
# Section: Scraping a One Main Page         #
#############################################

# == single page == {{{
url <- "https://boards.4chan.org/pol/"
page <-  read_html(url)
# $ ,rs = summary
# $ ,ra = arguments
# $ ,re = example
# :Rhelp topic

# board: e.g., /pol/ (there's only one, can't call '.text')
board_name <- page %>% html_nodes(".boardTitle") %>% html_text()

# Anonymous (ID: 2o5j+xkQ)  05/21/21(Fri)20:29:50 No.3...
post_id_date <- page %>% html_nodes(".post") %>% html_text()

# FIX: nested scraping
# post id: sticky thread numbers
sticky <- c()
for (s in html_nodes(page, "span.postNum") %>% html_text()) {
  x <- html_nodes(s, "img.stickyIcon")
  if (x == TRUE) {
    for (y in html_nodes(s, "a")) {
      y <- gsub("#(p|q)", "/", y)
      y <- gsub("thread/", "/", y)
      sticky <- c(sticky, y)
    }
  }
}

# post: number of sticky
# n_sticky = uniset([x.split("/")[0] for x in sticky])

# == helper functions == }}}

#' FUNCTION: rm_na
#' Replace na/null values in a list
#'
#' @param l2trans List to transform
#' @param t2repl Text to replace
rm_na <- function(l2trans, t2repl) {
  no_na <- lapply(l2trans, function(x) ifelse(is.null(x), t2repl, x))
  # need for both null/na cause didn't work on some lists
  no_na <- lapply(no_na, function(x) ifelse(is.na(x), t2repl, x))
}

#' FUNCTION: mb2kb
#' Convert MB to KB
#'
#' @param l2trans List to transform
mb2kb <- function (l2trans) {
  lapply(
    l2trans,
    function(x) {
      ifelse(grepl("MB", x),
        as.numeric(gsub("\\s?MB", "", x)) * 1000,
        gsub("\\s?KB", "", x)
      )
    }
  )
}

# }}} == helper functions ==

# == user == {{{
# usernames: e.g., Anonymous
usernames <- page %>% html_nodes("span.name") %>% html_text()

# posterid: e.g., 88OlJHyW
posteruid <- page %>% html_nodes("span.posteruid") %>% html_text()
posterid <- gsub("ID(?=:)|[(): ]", "", posteruid, perl = TRUE)

# country_flag: e.g., Austria
flag <- page %>% html_nodes("span.flag") %>% html_attr("title")
# }}} == user ==

# == image == {{{
# image_title: e.g., check catalog.jpg
img_title <- page %>% html_nodes(".fileText a") %>% html_text()

# image size in KB
img_size <- c()
for (s in html_nodes(page, ".fileText") %>% html_text()) {
  img_size <- c(img_size, str_extract(s, "(?<=\\()\\d+\\s(K|M)B"))
}

img_size <- unlist(mb2kb(rm_na(img_size, "0 KB")))

#' FUNCTION: img_dim
#' image dimensions: e.g, 400x400
#'  @param None
img_dim <- function() {
  idim <- c()
  for (s in html_nodes(page, ".fileText") %>% html_text()) {
    idim <- c(idim, str_extract(s, "(?<=,\\s)\\d+x\\d+"))
  }
}

idim <- img_dim()
# }}} == image ==

# == thread == {{{
# thread: e.g., thread/322626957#q322631216 -- #p = link; #q = reply
thread_post <- c()
for (s in html_nodes(page, "span.postNum a") %>% html_attr("href")) {
  thread_post <- c(thread_post, gsub("#(p|q)", "/", s) %>% gsub("thread/", "", .))
}

tmp_uniq <- unique(thread_post)
thread <- str_split(tmp_uniq, "/") %>% sapply(., "[[", 1)
post <- str_split(tmp_uniq, "/") %>% sapply(., "[[", 2)
# dictionary type object
thread_post_d <- list(names = thread, post)
# }}} == thread ==

# == OG post == {{{
# OP: Anonymous ## Mod    05/31/20(Sun)15:07:39 No. ...
post_op <- page %>% html_nodes(".post.op") %>% html_text()

# OP: post id
op_id <- str_extract(post_op, "(?<=No.)\\d+")
# }}} == OG post ==

# == replies == {{{
# NOTE: use brackets to pipe to an arg that is not the first
# save list to guarantee correct thread; probably better way to do this
nrep_nimg <- page %>%
  html_nodes("span.summary") %>%
  html_text() %>%
  { gsub("\\somitted. Click here to view.", "", .) } %>%
  { gsub("\\sand", ",", .)} %>%
  str_split(., ",")

# n_thread_reply <- str_split(nrep_nimg, ",") %>% sapply(., "[[", 1)
# n_img_reply <- str_split(nrep_nimg, ",") %>% sapply(., "[[", 2)

n_thread_reply <- map(nrep_nimg, 1)
n_img_reply <- map(nrep_nimg, 2)

thread_n <- page %>%
  html_nodes("span.summary a") %>%
  html_attr("href") %>%
  { gsub("thread/", "", .) }

xx <- rm_na(n_img_reply, '0 images')

# }}} == replies ==

# }}} == single page ==

# vim: ft=r:et:sw=0:ts=2:sts=2:fdm=marker:fmr={{{,}}}:
