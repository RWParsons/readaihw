#' Get data by measure category.
#'
#' @param measure_category_code A measure category code. See
#' `get_measure_categories()` for valid codes.
#' @param measure_code A measure code.
#' @param return_caveats A logical. Whether or not to return the caveats for
#' this data in a separate table. If `TRUE`, this will return a list with the
#' two tables.
#'
#' @return data
#' @export
#'
#' @examplesIf interactive() && curl::has_internet()
#' read_flat_data_extract(measure_category_code = "MYH-CANCER")
#' read_flat_data_extract(measure_category_code = "MYH-CANCER", measure_code = "MYH0001")
read_flat_data_extract <- function(measure_category_code, measure_code, return_caveats = FALSE) {
  assertthat::assert_that(assertthat::is.string(measure_category_code))
  assertthat::assert_that(measure_category_code %in% get_measure_categories()$measure_category_code)

  measure_code_str <- ""
  if (!missing(measure_code)) {
    assertthat::assert_that(assertthat::is.string(measure_code))
    assertthat::assert_that(measure_code %in% get_measures_from_category(measure_category_code)$measure_code)

    measure_code_str <- paste0("&measure_code=", measure_code)
  }

  max_rows <- 1000

  res <- call_myhosp_api(paste0("flat-data-extract/", measure_category_code, "?skip=0&top=5", measure_code_str))

  total_rows <- res$result$pagination$total_results_available

  skips <- seq(from = 0, to = total_rows - 1, by = max_rows)
  tops <- purrr::map(skips, ~ min(c(max_rows, total_rows - .x)))

  dframes <- purrr::map2(
    .x = skips, .y = tops,
    .f = ~ call_flat_data_segment(
      url = paste0("flat-data-extract/", measure_category_code),
      measure_code_str = measure_code_str,
      skip = .x,
      top = .y
    )
  )

  dframes |>
    dplyr::bind_rows() |>
    tidy_flat_data_extract(return_caveats = return_caveats)
}

tidy_resp_to_df <- function(result) {
  result |>
    purrr::map(unlist) |>
    dplyr::bind_rows() |>
    janitor::clean_names()
}

call_flat_data_segment <- function(url, skip, top, measure_code_str) {
  skip <- format(skip, scientific = FALSE)
  top <- format(top, scientific = FALSE)
  res <- call_myhosp_api(as.character(glue::glue("{url}?skip={skip}&top={top}{measure_code_str}")))
  tidy_resp_to_df(res$result$data)
}

tidy_flat_data_extract <- function(data, return_caveats) {
  if (any(stringr::str_detect(names(data), "caveat"))) {
    if ("caveat" %in% names(data)) {
      d_caveats <- data |>
        dplyr::select(dplyr::starts_with("caveat")) |>
        dplyr::filter(!is.na(caveat))

      data <- data |>
        dplyr::filter(is.na(caveat)) |>
        dplyr::select(-dplyr::starts_with("caveat"))
    } else {
      d_caveats <- data |>
        dplyr::select(
          dplyr::starts_with("caveat"),
          dplyr::starts_with("suppressions")
        ) |>
        dplyr::distinct()

      d_caveats <- d_caveats[!is.na(d_caveats[[1]]), ]

      data <- data |>
        dplyr::select(-dplyr::all_of(names(d_caveats)[-1])) |>
        dplyr::filter(is.na(!!rlang::sym(names(d_caveats)[1]))) |>
        dplyr::select(-dplyr::all_of(names(d_caveats)[1]))
    }
  }

  if (return_caveats) {
    res <- list(data = data, d_caveats = d_caveats)
  } else {
    res <- data
  }

  res
}
