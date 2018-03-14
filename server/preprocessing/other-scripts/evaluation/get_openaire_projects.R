#' Get EC projects
#'
#' @param grant_id Gets the project with the given grant identifier, if any
#' @param title Name of the project
#' @param acronym Acronym
#' @param call_id Search for projects by call identifier
#' @param start_year Gets the projects that started in the given year. Format: YYYY
#' @param end_year Gets the projects that ended in the given year. Format: YYYY
#' @param country Search for projects by participant countries. Foramt: 2-letter country code
#' @param org Search for projects by participant institutions (acronym)
#' @param limit number of projects to be returned
#' @param ... other API parameters \url{http://api.openaire.eu/}
#'
#' @import XML
#' @import httr
#' @export
#'
#' @examples \dontrun{
#' roa_projects(org = "UGOE")
#'
#' roa_projects(call_id = "FP7-PEOPLE-2010-IRSES")
#' }
roa_projects <- function(grant_id = NULL, publication_id = NULL, title = NULL,
  acronym = NULL, call_id = NULL, start_year = NULL, end_year = NULL,
  country = NULL, org = NULL, size = 1000, sort_by = NULL, sort_order = NULL,
  ...) {

  if (!is.null(sort_order)) {
    if (!is.null(sort_by)) {
      sort_by <- paste(sort_by, sort_order, sep = ",")
    }
  }
  args <- comp(list(grantID = grant_id, openairePublicationID = publication_id,
      name = title, acronym = acronym, callID = call_id, startYear = start_year,
      endYear = end_year, participantAcronyms = org,
      participantCountries = country, size = size, sortBy = sort_by,
      format = "xml"))
  assert_args(args)
  out <- tt_GET(path = "search/projects", query = args)
  res <- tt_parse(out, 'xml')
  tibble::as_tibble(
    do.call("rbind", lapply(parse_project(res), as.data.frame,
      stringsAsFactors = FALSE))
  )
}

xml_names <- c(
  grantID = "//code",
  acronym = "//acronym",
  title = "//title",
  startdate = "//startdate",
  enddate = "//enddate",
  callidentifier = "//callidentifier",
  ecsc39 = "//ecsc39",
  funding_level_0 = "//funding_level_0/name"
)

parse_project <- function(x) {
  results <- xml2::xml_find_all(x, xpath = '//results/result')
  lapply(results, function(z) {
    lapply(xml_names, function(w) {
      xml2::xml_text(xml2::xml_find_all(z, w))
    })
  })
}

# utility functions
oa_base <- function() "http://api.openaire.eu/"

`%||%` <- function(x, y) if (is.null(x)) y else x

comp <- function(x) Filter(Negate(is.null), x)

tt_GET <- function(path, query, ...) {
  if (is.null(path)) stop("No path provided")
  cli <- crul::HttpClient$new(url = oa_base(), opts = list(...))
  req <- cli$get(path = path, query = query)
  req$raise_for_status()
  return(req)
}

# fix double quotation in tsv
quote_fixing <- function(x){
  gsub('\"', '', x)
}

assert <- function(x, y, name = NULL) {
  if (!is.null(x)) {
    if (!inherits(x, y)) {
      if (is.null(name)) name <- deparse(substitute(x))
      stop(name, " must be of class ",
           paste0(y, collapse = ", "), call. = FALSE)
    }
  }
}

assert_arg <- function(x, y) {
  if (!is.null(x)) {
    if (!inherits(x, y)) {
      stop(sub("x\\$", "", deparse(substitute(x))), " must be of class ",
           paste0(y, collapse = ", "), call. = FALSE)
    }
  }
}

assert_args <- function(x) {
  assert_arg(x$size, c('numeric', 'integer'))
}

tt_parse <- function(x, format, raw = FALSE) {
  if (raw) return(x$parse("UTF-8"))
  switch(
    format,
    json = {
      tmp <- tryCatch(
        jsonlite::fromJSON(x$parse("UTF-8")),
        error = function(e) e
      )
      if (inherits(tmp, "error")) {
        stop("invalid JSON, try setting raw=TRUE")
      } else {
        return(tmp)
      }
    },
    tsv = suppressMessages(readr::read_tsv(x$content)),
    csv = suppressMessages(readr::read_csv(x$content)),
    xml = xml2::read_xml(x$content),
    stop("'format' must be of json, tsv, csv, or xml")
  )
}

check_format <- function(x) {
  assert(x, "character", "format")
  if (!x %in% c('json', 'xml', 'csv', 'tsv')) {
    stop("'format' must be one of json, xml, csv or tsv")
  }
}
