# clear and setup workspace
rm(list = ls())
library(rstudioapi)
options(warn=1)
wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(wd) #Don't forget to set your working directory

# setup packages
source("../vis_layout.R")
source("get_openaire_projects.R")
source('../openaire.R')

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
    write.table(output, file=paste0("../../../../examples/local_files/openaire/",
                                    params$org, "_", query, ".csv"), sep=",", row.names=FALSE)
  }
}

# run workflow
organizations <- read.csv("openaire.csv")
for (organization in organizations$org_openaire){
  org_projects <- unique(roa_projects(org=organization))
  org_projects <- org_projects[which(org_projects$funding_level_0 == 'FP7'),]
  by(org_projects, 1:nrow(org_projects), function(project) {produce_dataset(organization, project)})
}
