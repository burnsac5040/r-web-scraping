############################################################################
#    Author: Lucas Burns                                                   #
#     Email: burnsac@me.com                                                #
#     Title: scrape.R                                                      #
#   Created: 2021-05-26 11:32                                              #
############################################################################

# == imports == {{{
library(data.table)      # table manipulation
library(dplyr)           # data manipulation
library(splitstackshape) # splitting data.table
library(stringr)         # string operations
library(magrittr)        # includes '%>%' chain op
library(httr)            # web scraping
library(rvest)           # web scraping
library(XML)             # used in conj w/ rvest
library(purrr)           # vector ops, including 'map'
library(zeallot)         # %<-% operator for multiple var assignment
library(glue)            # like python f-string
# library(comprehenr) # like python list comprehension
# }}} == imports ==

# comprehenr e.g.
# List Comprehension Examples
# to_list(for(i in 1:10) if(i %% 2==0) i*i)
# to_list(for(i in 1:length(test)) if(i %% 5==0) test[i])
# test[seq(1, length(test), 5)]
# test[c(rep(FALSE, 5), TRUE)]

# help {{{
ls("package:dplyr")
help(unique)
# trim whitespace
trimws()
str_trim() # stringr
# }}} help

#############################################
# Section: Scraping a One Main Page         #
#############################################

# == helper functions == {{{

#' FUNCTION: rm_na
#' Replace na/null values in a list
#'
#' @param l2trans List to transform
#' @param t2repl Text to replace
rm_na <- function(l2trans, t2repl) {
  lapply(
    l2trans,
    function(x) {
      ifelse(is.null(x) || is.na(x), t2repl, x)
    }
  )
}

#' FUNCTION: mb2kb
#' Convert MB to KB
#'
#' @param l2trans List to transform
mb2kb <- function(l2trans) {
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

# == single page == {{{
url <- "https://boards.4chan.org/pol/"
page <-  read_html(url)
# $ ,rs = summary
# $ ,ra = arguments
# $ ,re = example
# :Rhelp topic

#' FUNCTION: gboard
#' board name: e.g., /pokemon/
gboard <- function(page) {
  return (html_nodes(page, ".boardTitle") %>%
    html_text())
}

board <- gboard(page)

#' FUNCTION: gwhole_post
#' Anonymous (ID: 2o5j+xkQ)  05/21/21(Fri)20:29:50 No.3...(content)
gwhole_post <- function(page) {
  return(html_nodes(page, ".post") %>%
      html_text() %>%
      trimws())
}

data.frame(post_whole)

post_whole <- gwhole_post(page)

#' FUNCTION: gsticky
#' Return hash of thread nums (k) with T/F if sticky (v)
gsticky <- function(page) {
  sticky_pointer <- c()
  i <- 0
  for (s in html_nodes(page, "span.postNum.desktop")) {
    if (grepl("Sticky", s)) {
      sticky_pointer <- c(sticky_pointer, "YES")
      i <- i + 1
    } else {
      sticky_pointer <- c(sticky_pointer, "NA")
      i <- i + 1
    }
  }

  sticky_scanner <- html_nodes(page, "span.postNum.desktop") %>% html_text()
  sticky_scanner <- gsub("No.|\\[Reply\\]|\\s*", "", sticky_scanner)
  stickies <- sticky_scanner[grepl("YES", sticky)]

  mlist <- mget(c("stickies", "sticky_scanner"))
  cva <- Reduce(intersect, mlist)
  overlap <- lapply(mlist, function(x) which(x %in% cva))

  map(
    sticky_scanner,
    function(x) {
      grepl(
        paste(sticky_scanner[overlap$sticky_scanner], collapse = "|"),
        x
      )
    }
  ) -> sticky_tf

  sticky_hash <- as.list(unlist(sticky_tf))
  names(sticky_hash) <- unlist(sticky_scanner)
  return(sticky_hash)
}

stickies <- gsticky(page)

# == user == {{{

#' FUNCTION: gusernames
#' user: Anonymous
gusernames <- function(page) {
  return (page %>% html_nodes("span.name") %>% html_text())
}

usernames <- gusernames(page)

#' FUNCTION: guid
#' userid: e.g., 88OlJHyW
guid <- function(page) {
  posteruid <- html_nodes(page, "span.posteruid") %>% html_text()
  return(gsub("ID(?=:)|[(): ]", "", posteruid, perl = TRUE))
}

uids <- guid(page)

#' FUNCTION: gflag
#' country_flag: e.g., Austria
gflag <- function(page) {
  return(html_nodes(page, "span.flag") %>% html_attr("title"))
}
flags <- gflag(page)
# }}} == user ==

# == image == {{{

#' FUNCTION: gimg_name
#' image name: e.g., "IRON_EAGLE.jpg"
gimg_name <- function(page) {
  return(html_nodes(page, ".fileText a") %>% html_text())
}

img_names <- gimg_name(page)

#' FUNCTION: gimg_size
#' image size in KB: e.g., 40
gimg_size <- function(page) {
  img_size <- c()
  for (s in html_nodes(page, ".fileText") %>% html_text()) {
    img_size <- c(img_size, str_extract(s, "(?<=\\()\\d+\\s(K|M)B"))
  }

  return(unlist(mb2kb(rm_na(img_size, "0 KB"))))
}

img_size <- gimg_size(page)

#' FUNCTION: gimg_dim
#' Image dimensions: e.g, 400x400
#'
#' @return width, height
gimg_dim <- function(page) {
  idim <- c()
  for (s in html_nodes(page, ".fileText") %>% html_text()) {
    idim <- c(idim, str_extract(s, "(?<=,\\s)\\d+x\\d+"))
  }
  idim <- str_split(idim, "x")
  return(list(map(idim, 1), map(idim, 2)))
}
# unlist(str_split(idim, "x"))[seq(1, length(idim), 2)]
# c(a, b) %<-% unlist(str_split(aa, "x"))

c(img_width, img_height) %<-% gimg_dim(page)
# }}} == image ==

# == thread == {{{

#' FUNCTION: gthread_post
#' @return data.table; thread_id & all post_ids
gthread_post <- function(page) {
  thread_post <- c()
  for (s in html_nodes(page, "span.postNum a") %>% html_attr("href")) {
    thread_post <- c(thread_post, gsub("#(p|q)", "/", s) %>% gsub("thread/", "", .))
  }

  tmp_uniq <- unique(thread_post)
  thread_id <- matrix(unlist(str_split(tmp_uniq, "/") %>% sapply(., "[[", 1)))
  post_id <- matrix(unlist(str_split(tmp_uniq, "/") %>% sapply(., "[[", 2)))

  ddf <- data.frame(thread_id,post_id)

  # stackoverflow.com/questions/33523320
  dtt <- setDT(ddf)[, do.call(paste, c(.SD, list(collapse=', '))), thread_id]
  dt1 <- cSplit(dtt, 'V1', sep='[ ,]+', fixed=FALSE, stripWhite=TRUE)
  setnames(dt1, 2:ncol(dt1), rep(names(ddf)[-1], 7))
  return(dt1)
}

thread_posts <- gthread_post(page)

data.frame(thread_posts)
# }}} == thread ==

# == OG post == {{{

#' FUNCTION: gpost_op
#' OP full post
#' OP id: Anonymous ## Mod    05/31/20(Sun)15:07:39 No. ...
gpost_op <- function(page) {
  post_op <- html_nodes(page, ".post.op") %>% html_text()
  op_id <- str_extract(post_op, "(?<=No.)\\d+") # op: post id
  return(list(post_op, op_id))
}

c(op_post, op_postid) %<-% gpost_op(page)
op_post_df <- data.frame(op_id=op_postid, op_post=op_post)

# }}} == OG post ==

# == replies == {{{

#' FUNCTION: greply_gimg
#' thread number
#' num replies: 3 replies
#' num reply images: 2 images
greply_gimg <- function(page) {
  nrep_nimg <- page %>%
    html_nodes("span.summary") %>%
    html_text() %>%
    { gsub("\\somitted. Click here to view.", "", .) } %>%
    { gsub("\\sand", ",", .)} %>%
    str_split(., ",")

  # n_thread_reply <- str_split(nrep_nimg, ",") %>% sapply(., "[[", 1)
  # n_img_reply <- str_split(nrep_nimg, ",") %>% sapply(., "[[", 2)

  n_thread_reply <- map(nrep_nimg, 1) %>%
      rm_na(., "0 replies") %>%
      { gsub("\\s?repl(ies|y)", "", .) } %>%
      trimws()

  n_img_reply <-  map(nrep_nimg, 2) %>%
      rm_na(., "0") %>%
      { gsub("\\s?image(s)?", "", .) } %>%
      trimws( )

  thread_n <- page %>%
      html_nodes("span.summary a") %>%
      html_attr("href") %>%
      { gsub("thread/", "", .) } %>%
      trimws()

  # n_img_reply <- rm_na(map(nrep_nimg, 2), "0 images")
  # n_thread_reply <- rm_na(map(nrep_nimg, 1), "0 replies")

  # return(list(thread_n, n_thread_reply, n_img_reply))
  return(
    data.frame(thread_id = threadn, n_replies = nreply, n_img_replies = nimg_reply)
  )
}

# c(threadn, nreply, nimg_reply) %<-% greply_gimg(page)
n_replyimg_df <- greply_gimg(page)

# diff sizes
n_replyimg_df$thread_id
op_post_df$op_id
# }}} == replies ==

# }}} == single page ==

# vim: ft=r:et:sw=0:ts=2:sts=2:fdm=marker:fmr={{{,}}}:
