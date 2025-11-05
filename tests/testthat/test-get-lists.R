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
    ~ expect_s3_class(
      suppressWarnings(get_measure_data_from_download_code(.x)),
      "data.frame"
    )
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
