#' Version checking for caching
#' @export
get_version_hash <- function() {
  tryCatch(
    {
      # Use existing functions to get counts
      dataset_count <- nrow(get_datasets(tidy_data = FALSE))
      measure_count <- nrow(get_measure_categories()) # Use categories as proxy

      metadata <- list(
        dataset_count = dataset_count,
        measure_count = measure_count,
        date = Sys.Date()
      )
      rlang::hash(metadata)
    },
    error = function(e) {
      # Fallback to date-based versioning if API calls fail
      warning("Could not generate version hash, using date: ", e$message)
      rlang::hash(Sys.Date())
    }
  )
}

#' Check if cached data is current
#'
#' @param file Path to cached data file
#' @return List with cached data if current, NULL if needs refresh
check_cached_data <- function(file) {
  if (!file.exists(file)) {
    return(NULL)
  }

  cached_data <- readRDS(file)
  current_hash <- get_version_hash()

  if (!is.null(cached_data$version_hash) && cached_data$version_hash == current_hash) {
    download_date <- if (!is.null(cached_data$downloaded)) {
      format(cached_data$downloaded, "%Y-%m-%d %H:%M")
    } else {
      "unknown"
    }
    message(glue::glue("{file} already exists and is the latest release (downloaded: {download_date}), use download_all_aihw_data(force=TRUE) to force download even if file exists and is current"))
    return(cached_data)
  }

  message("Data version changed - downloading fresh data")
  return(NULL)
}
