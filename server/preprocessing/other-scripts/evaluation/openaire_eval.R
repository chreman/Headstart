# clear and setup workspace
rm(list = ls())
library(rstudioapi)
options(warn=1)
wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(wd) #Don't forget to set your working directory

# setup packages
source("../vis_layout.R")
source('../openaire.R')

# set params
debug = FALSE
MAX_CLUSTERS = 15
ADDITIONAL_STOP_WORDS = "english"

targets <- read.csv("openaire.csv")

for (t in targets$org_openaire){
  target_projects <- unique(roa_projects(org=t))
  acronym <- target_projects$acronym
  project_id <- target_projects$grantID
  funding_level <- target_projects$funding_level_0
  query <- acronym
  params <- list('project_id'=project_id, 'funding_level'=funding_level)
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
  print(head(output), 3)
}
