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

produce_dataset <- function(project){
  acronym <- project$acronym
  project_id <- project$grantID
  funding_level <- project$funding_level_0
  query <- acronym
  params <- list('project_id'=project_id, 'funding_level'=funding_level)
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
  output_json = vis_layout(input_data$text, input_data$metadata,
                           max_clusters=MAX_CLUSTERS,
                           add_stop_words=ADDITIONAL_STOP_WORDS,testing=TRUE, list_size=-1)
  output <- fromJSON((output_json))
  print(output$area)
}

# run workflow
targets <- read.csv("openaire.csv")
for (target in targets$org_openaire){
  target_projects <- unique(roa_projects(org=target))
  target_projects <- target_projects[which(target_projects$funding_level_0 == 'FP7'),]
  print(target_projects)
  by(target_projects, 1:nrow(target_projects), function(project) {produce_dataset(project)})
}
