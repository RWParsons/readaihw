remake_fixtures <- FALSE

if (remake_fixtures) {
  save_fixture <- function(x, fname) {
    saveRDS(x, file.path(test_path(), "fixtures", paste0(fname, ".rds")))
  }

  # remove all existing fixtures
  purrr::map(list.files(file.path(test_path(), "fixtures"), full.names = TRUE), ~ file.remove(.x))

  save_fixture(get_datasets(), "get_datasets")
  save_fixture(get_datasets(tidy_data = FALSE), "get_datasets_untidy")
  save_fixture(call_myhosp_api("datasets/904/data-items"), "dataset-904-api-resp")
  save_fixture(get_measures(), "get_measures")
  save_fixture(get_reporting_units(), "get_reporting_units")
  sample_flat_data <- get_all_flat_data() |> dplyr::slice_head(n = 100)
  save_fixture(sample_flat_data, "sample_flat_data")

  dataset_ids <- c(
    "904", "1329", "1336", "2735",
    "2742", "3515", "8115", "8202",
    "8289", "8983", "9658", "10349"
  )
  purrr::map(
    dataset_ids,
    ~ save_fixture(read_dataset_by_id(.x), glue::glue("dataset-{.x}"))
  )
}
