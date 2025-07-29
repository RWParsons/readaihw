test_that("get_version_hash() works", {
  skip_if_offline()

  hash <- get_version_hash()
  expect_type(hash, "character")
  expect_equal(nchar(hash), 32) # MD5 hash length
})

test_that("get_version_hash() handles errors gracefully", {
  local_mocked_bindings(
    get_datasets = function(...) stop("Network error"),
    get_measure_categories = function(...) stop("API error")
  )

  expect_warning(hash <- get_version_hash(), "Could not generate version hash")
  expect_type(hash, "character")
  expect_equal(nchar(hash), 32)
})

test_that("check_cached_data() works with non-existent file", {
  temp_file <- tempfile(fileext = ".rds")
  result <- check_cached_data(temp_file)
  expect_null(result)
})

test_that("check_cached_data() detects outdated cache", {
  temp_file <- tempfile(fileext = ".rds")

  # Create fake cached data with old hash
  fake_data <- list(
    datasets = data.frame(id = 1:3),
    version_hash = "old_hash_12345",
    downloaded = Sys.time() - 3600
  )
  saveRDS(fake_data, temp_file)

  # Mock get_version_hash to return different hash
  local_mocked_bindings(
    get_version_hash = function() "new_hash_67890"
  )

  expect_message(result <- check_cached_data(temp_file), "Data version changed")
  expect_null(result)

  unlink(temp_file)
})
