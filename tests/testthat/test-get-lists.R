test_that("list getters", {
  skip_if_offline()

  expect_s3_class(get_measure_categories(), "data.frame")
  expect_s3_class(get_datasets(), "data.frame")
  expect_s3_class(get_measure_download_codes(), "data.frame")
})


test_that("get_measure_data() works with get_measure_download_codes() codes", {
  skip_if_offline()

  codes <- get_measure_download_codes()$datasheet_code

  # test that it works for 3 randomly selected codes
  purrr::map(
    sample(codes, size = 3),
    ~ expect_s3_class(suppressWarnings(get_measure_data(.x)), "data.frame")
  )
})


test_that("get_measure_data() works with get_measure_download_codes() codes", {
  skip_if_offline()

  # picking out the second one - currently cancer - because it loads faster
  # than others
  cat_code <- get_measure_categories()$measure_category_code[2]

  # test that it works for 3 randomly selected codes

  d_category <- read_flat_data_extract(measure_category_code = cat_code)
  expect_s3_class(d_category, "data.frame")

  d_category_measure <- read_flat_data_extract(
    measure_category_code = cat_code,
    measure_code = sample(d_category$measure_code, size = 1)
  )
  expect_s3_class(d_category_measure, "data.frame")
})

test_that("get_all_flat_data() works offline with mocked data", {
  source(test_path("helper-get-fixture.R"))

  local_mocked_bindings(
    get_measure_categories = function() {
      data.frame(
        measure_category_code = c("MYH-CANCER", "MYH-ED"),
        stringsAsFactors = FALSE
      )
    },
    read_flat_data_extract = function(category_code, ...) {
      data.frame(
        category = category_code,
        value = runif(5),
        reporting_unit_code = paste0("UNIT", 1:5),
        stringsAsFactors = FALSE
      )
    }
  )

  result <- httptest2::without_internet(get_all_flat_data())
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("category" %in% names(result))
})

test_that("get_measures() works offline with mocked data", {
  local_mocked_bindings(
    get_measure_categories = function() {
      data.frame(
        measure_category_code = c("MYH-CANCER", "MYH-ED"),
        stringsAsFactors = FALSE
      )
    },
    get_measures_from_category = function(category_code, trim = FALSE) {
      data.frame(
        measure_code = paste0(category_code, "_001"),
        measure_name = paste("Test measure for", category_code),
        category = category_code,
        stringsAsFactors = FALSE
      )
    }
  )

  result <- httptest2::without_internet(get_measures())
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("measure_code" %in% names(result))
})
