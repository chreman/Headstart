# clear and setup workspace
rm(list = ls())
library(rstudioapi)
options(warn=1)
wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(wd) #Don't forget to set your working directory

# setup packages
library('ropenaire')
source("../vis_layout.R")
source('../openaire.R')
source('../utils.R')

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

get_metrics <- function(query, params, output) {
  tmp <- c(query=query, unlist(params))
  tmp <- data.frame(t(tmp))
  tmp$missing_dois <- sum(output$doi == "")
  tmp$n_papers <- length(output$id)
  eval_metrics <<- rbind.fill(eval_metrics, tmp)
}

export_project_vis <- function(query, params){
  input_data = get_papers(query, params)
  if (nrow(input_data$metadata) > 2){
    output_json = vis_layout(input_data$text, input_data$metadata,
                             max_clusters=MAX_CLUSTERS,
                             add_stop_words=ADDITIONAL_STOP_WORDS,testing=TRUE, list_size=-1)
    output = fromJSON((output_json))
    output$x <- vapply(output$x, paste, collapse = ", ", character(1L))
    output$y <- vapply(output$y, paste, collapse = ", ", character(1L))
    output$area_uri <- vapply(output$area_uri, paste, collapse = ", ", character(1L))
    output$cluster_labels <- vapply(output$cluster_labels, paste, collapse = ", ", character(1L))
    output$readers <- ""
    output$file_hash <- ""
    get_metrics(query, params, output)
    write.table(output, file=paste0("../../../../examples/local_files/openaire/",
                                    params$org, "_", query, ".csv"), sep=",", row.names=FALSE)
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
