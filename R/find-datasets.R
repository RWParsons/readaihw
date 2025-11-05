#' Find the available measures within a measure category.
#'
#' @param measure_category_code A measure category code. See
#' `get_measure_categories()` for a list.
#' @param trim Whether or not to exclude the potentially unnecessary variables.
#'
#' @return data
#' @export
#'
#' @examplesIf interactive() && curl::has_internet()
#' get_measures_from_category("MYH-CANCER")
get_measures_from_category <- function(measure_category_code, trim = TRUE) {
  url <- as.character(glue::glue(
    "measure-categories/{measure_category_code}/measures"
  ))
  d <- call_myhosp_api(url)$result |>
    tidy_resp_to_df()

  if (trim) {
    d <- d |>
      dplyr::select(measure_code, measure_name, units = units_units_name)
  }

  d
}


#' Read datasets by the measure code.
#'
#' @param measure_code A measure code. For example, "MYH0001".
#'
#' @returns data
#' @export
#'
#' @examplesIf interactive() && curl::has_internet()
#' get_measure_data("MYH0001")
get_measure_data <- function(measure_code) {
  url <- as.character(glue::glue("measures/{measure_code}/data-items"))
  call_myhosp_api(url)$result |>
    tidy_resp_to_df()
}


#' Read datasets by the reported measure code.
#'
#' @param reported_measure_code A reported measure code. For example, "MYH-RM0037".
#'
#' @returns data
#' @export
#'
#' @examplesIf interactive() && curl::has_internet()
#' get_reported_measure_data("MYH-RM0037")
get_reported_measure_data <- function(reported_measure_code) {
  url <- as.character(glue::glue(
    "reported-measures/{reported_measure_code}/data-items"
  ))
  call_myhosp_api(url)$result |>
    tidy_resp_to_df()
}


#' Read datasets by the reporting unit code.
#'
#' @param reporting_unit_code A measure code. For example, "H0012".
#'
#' @returns data
#' @export
#'
#' @examplesIf interactive() && curl::has_internet()
#' get_reporting_unit_data("H0012")
get_reporting_unit_data <- function(reporting_unit_code) {
  url <- as.character(glue::glue(
    "reporting-units/{reporting_unit_code}/data-items"
  ))
  call_myhosp_api(url)$result |>
    tidy_resp_to_df()
}


#' Read (multiple) datasets by id
#'
#' @param ids id of the dataset. See `get_datasets()`.
#' @param return_caveats A logical. Whether or not to return the caveats for
#' this data in a separate table. If `TRUE`, this will return a list with the
#' two tables.
#' @param tidy_data A logical. Whether or not to tidy the data (rename
#' columns and remove columns which only have one unique value).
#'
#' @return data
#' @export
#'
#' @examplesIf interactive() && curl::has_internet()
#' read_dataset_ids(c(1, 2))
read_dataset_ids <- function(ids, return_caveats = FALSE, tidy_data = TRUE) {
  d_datasets <- get_datasets(tidy_data = FALSE) |>
    dplyr::filter(data_set_id %in% ids)

  if (any(!ids %in% d_datasets$data_set_id)) {
    missing_ids <- ids[!ids %in% d_datasets$data_set_id] |>
      paste0(collapse = ", ")
    warning(glue::glue(
      "IDs passed were not valid (available in `get_datasets()`): {missing_ids}"
    ))
  }

  dframes <- d_datasets$data_set_id |>
    purrr::map(read_dataset_by_id) |>
    dplyr::bind_rows() |>
    tidy_flat_data_extract(return_caveats = return_caveats)

  l <- list()

  if (return_caveats) {
    d_combined <- dplyr::left_join(d_datasets, dframes$data, by = "data_set_id")
    l$caveats <- dframes$d_caveats
  } else {
    d_combined <- dplyr::left_join(d_datasets, dframes, by = "data_set_id")
  }

  if (tidy_data) {
    d_combined <- tidy_dataset(d_combined)
  }

  if (return_caveats) {
    l$data <- d_combined
    return(l)
  }

  d_combined
}

read_dataset_by_id <- function(id) {
  url <- as.character(glue::glue("datasets/{id}/data-items"))
  call_myhosp_api(url)$result |> tidy_resp_to_df()
}

tidy_dataset <- function(dataset) {
  dataset |>
    rename_dataset() |>
    drop_outcome_if_constant()
}

drop_outcome_if_constant <- function(dataset) {
  # if every row in the dataset has the same outcome measure (code/name) then
  # rename the `value` column to this outcome name and remove the outcome name.
  if (length(unique(dataset$outcome_measure_name)) == 1) {
    outcome_cols <- c("outcome_measure_name", "outcome_measure_code")

    outcome <- janitor::make_clean_names(unique(dataset$outcome_measure_name))
    dataset <- dataset |>
      dplyr::rename(!!rlang::sym(outcome) := value) |>
      dplyr::select(-dplyr::all_of(outcome_cols))
  }
  dataset
}

rename_dataset <- function(dataset) {
  lkp <- c(
    data_id = "data_set_id",
    data_name = "data_set_name",
    start_date = "reporting_start_date",
    end_date = "reporting_end_date",
    outcome_measure_code = "reported_measure_summary_measure_summary_measure_code",
    outcome_measure_name = "reported_measure_summary_measure_summary_measure_name",
    reported_measure_code = "reported_measure_summary_reported_measure_code",
    reported_measure_name = "reported_measure_summary_reported_measure_name",
    unit_class_code = "peer_group_summary_reporting_unit_code",
    unit_class_name = "peer_group_summary_reporting_unit_name",
    unit_reporting_type_code = "peer_group_summary_reporting_unit_type_reporting_unit_type_code",
    unit_reporting_type_name = "peer_group_summary_reporting_unit_type_reporting_unit_type_name",
    unit_code = "reporting_unit_summary_reporting_unit_code",
    unit_name = "reporting_unit_summary_reporting_unit_name",
    unit_type_code = "reporting_unit_summary_reporting_unit_type_reporting_unit_type_code",
    unit_type_name = "reporting_unit_summary_reporting_unit_type_reporting_unit_type_name",
    "value"
  )

  dplyr::select(dataset, dplyr::any_of(lkp))
}
