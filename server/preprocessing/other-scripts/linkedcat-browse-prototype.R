library(solrium)
library(plyr)


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
  search_res = res$search
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
  return(metadata)
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

corpus <- get_corpus()
