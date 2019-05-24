library(solrium)
library(plyr)
library(logging)
library(tidyverse)
setwd("/home/chris/projects/OpenKnowledgeMaps/Headstart/server/preprocessing/other-scripts/")
source("vis_layout.R")
source("utils.R")

lclog <- getLogger('api.linkedcat')

get_corpus <- function(){
  host=paste0(Sys.getenv("LINKEDCAT_USER"),":",Sys.getenv("LINKEDCAT_PWD"),"@",Sys.getenv("LINKEDCAT_SOLR"))
  conn <- SolrClient$new(host=host,
                         path="solr/linkedcat2", port=NULL, scheme="https")

  q_params <- build_query(query, params, limit)
  res <- solr_all(conn, "linkedcat2", params = q_params, concat="; ")

  if (nrow(res$search) == 0){
    stop(paste("No results retrieved."))
  }


  # make results dataframe
  search_res <- res$search
  search_res <- search_res %>% drop_na(bkl_top_caption, bkl_caption)
  metadata <- data.frame(search_res$id)
  names(metadata) <- c('id')

  metadata$subject <- if (!is.null(search_res$keyword_a)) unlist(lapply(search_res$keyword_a, function(x) {gsub("; $", "", x)})) else ""
  metadata$subject <- unlist(lapply(metadata$subject, function(x) {gsub("; ; ", "; ", x)}))
  metadata$authors <- paste(search_res$author100_a, search_res$author700_a, sep="; ")
  metadata$authors <- unlist(lapply(metadata$authors, function(x) {gsub("; $|,$", "", x)}))
  metadata$authors <- unlist(lapply(metadata$authors, function(x) {gsub("^; ", "", x)}))
  metadata$author_date <- metadata$author100_d
  metadata$title <- if (!is.null(search_res$main_title)) search_res$main_title else ""
  metadata$year <- search_res$pub_year
  metadata$readers <- 0
  metadata$url <- search_res$id
  metadata$link <- "" # needs fix
  metadata$oa_state <- 1
  metadata$subject_orig = metadata$subject
  metadata$relevance = c(nrow(metadata):1)
  metadata$bkl_caption = if (!is.null(search_res$bkl_caption)) search_res$bkl_caption else ""
  metadata$bkl_top_caption = if (!is.null(search_res$bkl_top_caption)) search_res$bkl_top_caption else ""

  text = data.frame(matrix(nrow=nrow(metadata)))
  text$id = metadata$id
  # Add all keywords, including classification to text
  text$content = paste(search_res$bkl_top_caption, search_res$bkl_caption,
                       sep = " ")


  ret_val=list("metadata" = metadata, "text" = text)

  return(ret_val)
}

build_query <- function(query, params, limit){
  q <- "*:*"
  # fields to return
  r_fields <- c('id', 'idnr',
    'content_type_a', 'content_type_2',
    'main_title', 'subtitle', 'pub_year',
    'host_label', 'host_maintitle', 'host_pubplace', 'host_pubyear',
    'author100_a', 'author100_d', 'author100_0', 'author100_4',
    'author700_a', 'author700_d', 'author700_0',
    'bkl_caption', 'bkl_top_caption',
    'keyword_a', 'tags', 'category', 'bib', 'language_code')
  q_params <- list(q = q, rows = 99999, fl = r_fields)
}

# input_data <- get_corpus()
#
# bkls <- (input_data$metadata
#           %>% separate_rows(bkl_top_caption, sep="; ")
#           %>% separate_rows(bkl_caption, sep="; ")
#           %>% group_by(bkl_top_caption, bkl_caption)
#           %>% count()
#           %>% arrange(desc(n), desc(bkl_top_caption))
#           %>% drop_na())
#
# DEBUG <- TRUE
#
# output_json = vis_layout(input_data$text, input_data$metadata,
#                          api="linkedcat",
#                          max_clusters = 15,
#                          lang = "german",
#                          add_stop_words = "german",
#                          testing=FALSE, list_size=-1)

valid_langs <- list(
   'ger'='german'
)
