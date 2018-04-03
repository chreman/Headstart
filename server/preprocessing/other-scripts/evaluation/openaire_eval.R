# clear and setup workspace
rm(list = ls())
library(rstudioapi)
options(warn=1)
wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(wd) #Don't forget to set your working directory

# setup packages
library('ropenaire')
source("../vis_layout.R")
source("../altmetrics.R")
source('../openaire.R')
source('../utils.R')
library(NbClust)
library("ggplot2")
library("GGally")
library("reshape2")

# set params
debug = FALSE
MAX_CLUSTERS = 15
ADDITIONAL_STOP_WORDS = "english"

produce_dataset <- function(org, project){
  acronym <- project$acronym
  project_id <- project$grantID
  funding_level <- project$funding_level_0
  query <- acronym
  params <- list('project_id'=project_id,
                 'funding_level'=funding_level,
                 'org'=org)
  print(paste(query, params$project_id, params$funding_level))
  tryCatch({
    export_project_vis(query, params)
  }, error = function(err){
    print(err)
  }, finally = {
  })
}

get_statistics <- function(query, params, input_data, output) {
  n_papers <- length(output$id)
  tmp <- c(query=query, unlist(params))
  tmp <- data.frame(t(tmp))
  tmp$missing_dois <- sum(output$doi == "")
  tmp$n_papers <- n_papers
  coverage <- 1 - (colSums(
                      mapply(is.na,
                             output[c('cited_by_tweeters_count',
                                      'readers.mendeley',
                                      'citation_count')]))
                   / nrow(output))
  tmp <- cbind(tmp, t(coverage))
  process_metrics <- get_process_metrics(input_data$metadata, input_data$text)
  tmp <- cbind(tmp, t(process_metrics))
  eval_metrics <<- rbind.fill(eval_metrics, tmp)
}

get_process_metrics <- function(metadata, text){
  stops <- stopwords("english")
  result <- create_tdm_matrix(metadata, text, stops)
  tdm_matrix <- result$tdm_matrix
  normalized_matrix <- normalize_matrix(tdm_matrix)

  nb1 <- NbClust(method="ward.D", index="silhouette",
                 data = tdm_matrix, diss = normalized_matrix,
                 distance=NULL, min.nc = 1, max.nc = min(20, nrow(tdm_matrix)-1))
  nb2 <- NbClust(method="ward.D", index="sdindex",
                 data = tdm_matrix, diss = normalized_matrix,
                 distance=NULL, min.nc = 1, max.nc = min(20, nrow(tdm_matrix)-1))
  nb3 <- NbClust(method="ward.D", index="cindex",
                 data = tdm_matrix, diss = normalized_matrix,
                 distance=NULL, min.nc = 1, max.nc = min(20, nrow(tdm_matrix)-1))
  nb4 <- NbClust(method="ward.D", index="ptbiserial",
                 data = tdm_matrix, diss = normalized_matrix,
                 distance=NULL, min.nc = 1, max.nc = min(20, nrow(tdm_matrix)-1))

  ordination <- create_ordination(normalized_matrix)
  stress <- min(nm$stress)
  R2 <- max(nm$r2)
  return (list(R2 = R2, stress = stress,
               k_silhouette = nb1$Best.nc[1], i_silhouette = nb1$Best.nc[2],
               k_sdindex = nb2$Best.nc[1], i_sdindex = nb2$Best.nc[2],
               k_cindex = nb3$Best.nc[1], i_cindex = nb3$Best.nc[2],
               k_ptbiserial = nb4$Best.nc[1], i_ptbiserial = nb4$Best.nc[2]))
}

export_project_vis <- function(query, params){
  input_data = get_papers(query, params)
  if (nrow(input_data$metadata) > 2){
    output_json = vis_layout(input_data$text, input_data$metadata,
                             max_clusters=MAX_CLUSTERS,
                             add_stop_words=ADDITIONAL_STOP_WORDS,testing=TRUE, list_size=-1)
    output_json = enrich_output_json(output_json)
    output = fromJSON(output_json)
    output$x <- vapply(output$x, paste, collapse = ", ", character(1L))
    output$y <- vapply(output$y, paste, collapse = ", ", character(1L))
    output$area_uri <- vapply(output$area_uri, paste, collapse = ", ", character(1L))
    output$cluster_labels <- vapply(output$cluster_labels, paste, collapse = ", ", character(1L))
    output$readers <- ""
    output$file_hash <- ""
    get_statistics(query, params, input_data, output)
    write.table(output, file=paste0("../../../../examples/local_files/openaire/",
                                    gsub(" ", "_", params$org), "_", query, ".csv"), sep=",", row.names=FALSE)
  }
}

# run workflow
organizations <- read.csv("openaire.csv")
total_projects <- data.frame()
eval_metrics <- data.frame()
for (organization in organizations$org_openaire){
  org_projects <- unique(roa_projects(org=organization))
  org_projects <- org_projects[which(org_projects$funding_level_0 == 'FP7'),]
  org_projects$organization <- organization
  by(org_projects, 1:nrow(org_projects), function(project) {produce_dataset(organization, project)})
  total_projects <- rbind.fill(total_projects, org_projects)
}
write.table(total_projects, file="openaire_projects.csv", sep=",", row.names=FALSE)
write.table(eval_metrics, file="openaire_eval_metrics.csv", sep=",", row.names=FALSE)

print(paste("Mean missing dois:", mean(eval_metrics$missing_dois/eval_metrics$n_papers)))
print(paste("Median missing dois:", median(eval_metrics$missing_dois/eval_metrics$n_papers)))
print("Mean altmetrics coverage:")
print(colSums(eval_metrics[c('cited_by_tweeters_count', 'readers.mendeley', 'citation_count')])/nrow(eval_metrics))


hexbin_mapper <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) +
    geom_hex(bins=7)
  p
}

for_pairplot <- eval_metrics[, c('R2', 'stress',
                                 'k_silhouette', 'i_silhouette',
                                 'k_sdindex', 'i_sdindex',
                                 'k_cindex', 'i_cindex',
                                 'k_ptbiserial', 'i_ptbiserial')]
for_pairplot <- data.frame(apply(for_pairplot, MARGIN = 2, FUN = unlist))
ggpairs(for_pairplot, lower = list(continuous = hexbin_mapper))

n_obs <- function(x){return(c(y=median(x)+10, label=mean(x)))}

for_boxplot1 <- eval_metrics[, c('org', 'missing_dois')]
for_boxplot2 <- eval_metrics[, c('org', 'n_papers')]
for_boxplot3 <- eval_metrics[, c('org', 'cited_by_tweeters_count', 'readers.mendeley', 'citation_count')]
(ggplot(melt(for_boxplot1), aes(x=variable, y=value))
    + geom_boxplot()
    + stat_summary(fun.data = n_obs, geom="text", fun.y=median))
(ggplot(melt(for_boxplot2), aes(x=variable, y=value))
  + geom_boxplot()
  + stat_summary(fun.data = n_obs, geom="text", fun.y=median))
(ggplot(melt(for_boxplot3), aes(x=variable, y=value))
  + geom_boxplot()
  + stat_summary(fun.data = n_obs, geom="text", fun.y=median)
  + coord_cartesian(ylim = c(0, 1)))
