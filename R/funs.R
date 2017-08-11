#' get_trumptwitterarchive
#'
#' Returns data from trumptwitterarchive.com.
#'
#' @param years Years from which to collect data. Defaults (NULL, TRUE, or "all")
#'   to 2008-current year.
#' @return Returns data frame (tbl) of status IDs with "data" attribute consisting of
#'   list of data by year.
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @examples
#' \dontrun{
#' ## get data
#' status_ids <- get_trumptwitterarchive()
#'
#' ## preview data
#' status_ids
#'
#' ## get full data archive
#' data <- trumptwitterarchive_data(status_ids)
#'
#' ## preview data
#' str(data, 2)
#' }
#' @export
get_trumptwitterarchive <- function(years = NULL) {
  ## default to years 2008 through current
  if (is.null(years) || isTRUE(years) || identical(tolower(years), "all")) {
    years <- seq(2008, as.integer(format(Sys.Date(), "%Y")))
  }
  stopifnot(is.numeric(years))
  ## get data for each year
  tta <- lapply(years, trumptwitterarchive_)
  ## make status IDs data frame with tta as attribute
  ids <- lapply(tta, "[[", "id_str")
  ids <- tibble::as_tibble(
    list(status_id = unlist(ids)),
    validate = FALSE
  )
  ## list of data (element = year)
  attr(ids, "data") <- tta
  attr(ids, "years") <- years
  ids
}

#' trumptwitterarchive_data
#'
#' Extracts full trumptwitterarchive data
#'
#' @param data Data frame returned by \code{\link{get_trumptwitterarchive}} with
#'   "data" attribute.
#' @param years Optional integer used to subset data to return only certain years.
#'   Defaults to NULL, which means all data is returned.
#' @return A tbl of data from trumptwitterarchive.com.
#' @importFrom rtweet format_date
#' @export
trumptwitterarchive_data <- function(data, years = NULL) {
  if (!"data" %in% names(attributes(data))) {
    stop("Archive data not found", call. = FALSE)
  }
  tta <- attr(data, "data")
  
  if (!is.null(years)) {
    ## if years to subset are provided
    data_years <- attr(data, "years")
    ## if no years attr or if years length differs return w/ warning
    if (is.null(data_years) || length(data_years) != length(years)) {
      warning(
        "Length of years attribute differs from length of data. Returning all extracted data",
        call. = FALSE
      )
    } else {
      tta <- tta[data_years %in% years]
    }
  }
  ## tidy things up a bit
  tta <- tta[lengths(tta) > 0L]
  if (is.list(tta)) {
    tta <- lapply(tta, tibble::as_tibble)
    tta <- do.call("rbind", tta)
    if ("created_at" %in% names(tta)) {
      tta$created_at <- rtweet:::format_date(tta$created_at)
    }
  }
  tta
}



#' trumptwitterarchive_
#'
#' Internal function used to retrieve trumptwitterarchive data
#'
#' @param year Integer, specifying year of data to return.
#' @param fromJSON Logical, indicating whether to convert repsonse object to
#'   nested list object.
#' @return Response object from trumptwitterarchive request converted (by default)
#'   to R-friendly list object.
#' @importFrom httr content GET
#' @importFrom jsonlite fromJSON
#' @noRd
#' @keywords internal
trumptwitterarchive_ <- function(year, fromJSON = TRUE) {
  ## build and send request
  url <- paste0(
    "http://trumptwitterarchive.com/",
    "data/realdonaldtrump/",
    year,
    ".json"
  )
  op <- getOption("encoding")
  on.exit(options(encoding = op))
  options(encoding = "UTF-8")
  ## response object
  r <- tryCatch(httr::GET(url), error = function(e) return(NULL))
  if (is.null(r)) {
    warning(
      paste("http request for", year, "data failed."),
      call. = FALSE
    )
    return(data.frame())
  }  
  ## check html status
  httr::warn_for_status(r)
  ## if fromJSON then convert to list otherwise return response object
  if (fromJSON) {
    r <- httr::content(r, "text", encoding = "UTF-8")
    ## if html return empty data frame
    if (grepl("^<!DOCTYPE", r)) {
      r <- data.frame()
    } else {
      r <- jsonlite::fromJSON(r)
    }
  }
  r
}
