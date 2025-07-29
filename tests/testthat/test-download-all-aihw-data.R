test_that("download_all_aihw_data() creates expected structure", {
  temp_file <- tempfile(fileext = ".rds")

  # Mock all the API calls
  local_mocked_bindings(
    get_datasets = function(...) data.frame(id = 1:2, name = c("A", "B")),
    get_measures = function() data.frame(code = c("M1", "M2")),
    get_reporting_units = function() data.frame(unit = c("U1", "U2")),
    get_measure_categories = function() data.frame(category = c("C1", "C2")),
    get_hospital_mappings = function() data.frame(hospital = c("H1", "H2")),
    get_all_flat_data = function() data.frame(value = c(1, 2)),
    get_version_hash = function() "test_hash_123"
  )

  result <- httptest2::without_internet(
    download_all_aihw_data(file = temp_file, force = TRUE)
  )

  # Check structure
  expect_type(result, "list")
  expected_elements <- c(
    "datasets", "measures", "reporting_units",
    "measure_categories", "hospital_mappings",
    "flat_data", "downloaded", "version_hash"
  )
  expect_true(all(expected_elements %in% names(result)))

  # Check file was created
  expect_true(file.exists(temp_file))

  # Check cached data detection works
  expect_message(
    result2 <- download_all_aihw_data(file = temp_file, force = FALSE),
    "already exists and is the latest release"
  )

  unlink(temp_file)
})
