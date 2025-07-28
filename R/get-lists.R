#' Get mappings and locations for hospitals.
#'
#' @return data
#' @export
#'
#' @examplesIf interactive() && curl::has_internet()
#' get_hospital_mappings()
get_hospital_mappings <- function() {
  call_myhosp_api("reporting-units-downloads/mappings")
}

#' Get set of measure categories which can be used in `read_flat_data_extract()`.
#'
#' @return data
#' @export
#'
#' @examplesIf interactive() && curl::has_internet()
#' get_measure_categories()
get_measure_categories <- function() {
  res <- call_myhosp_api("measure-categories")
  tidy_resp_to_df(res$result)
}

#' Show available datasets.
#'
#' @param tidy_data A logical. Whether or not to tidy the data (rename
#' columns and remove columns which only have one unique value).
#'
#' @return data
#' @export
#'
#' @examplesIf interactive() && curl::has_internet()
#' get_datasets()
get_datasets <- function(tidy_data = TRUE) {
  res <- call_myhosp_api("datasets")
  d <- tidy_resp_to_df(res$result)

  if (tidy_data) {
    d <- rename_dataset(d)
  }

  d
}

#' Get set of measure download codes that can be used in `get_measure_data()`.
#'
#' @return data
#' @export
#'
#' @examplesIf interactive() && curl::has_internet()
#' get_measure_download_codes()
get_measure_download_codes <- function() {
  res <- call_myhosp_api("measure-downloads/measure-download-codes")
  tidy_resp_to_df(res$result)
}


#' Get data by measure download code.
#'
#' @param measure_download_code A measure download code. See `datasheet_code` in
#' `get_measure_download_codes()`.
#'
#' @return data
#' @export
#'
#' @examplesIf interactive() && curl::has_internet()
#' get_measure_data("myh-adm")
get_measure_data <- function(measure_download_code) {
  assertthat::assert_that(assertthat::is.string(measure_download_code))
  assertthat::assert_that(measure_download_code %in% get_measure_download_codes()$datasheet_code)

  call_myhosp_api(paste0("measure-downloads/", measure_download_code))
}
