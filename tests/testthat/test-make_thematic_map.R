test_that("raises error when no sf provided", {
  test_data <- data.frame(region=c("BZN", "BZE", "BZC"), n=c(10,20,30))

  expect_error(make_thematic_map(test_data, n),
               "If your data is not already an sf data frame, you must provide a map to join it to.")
})

test_that("raises error when nothing specified to join map and data", {
  test_data <- data.frame(region=c("BZN", "BZE", "BZC"), n=c(10,20,30))

  expect_error(make_thematic_map(test_data, n, .map=tdwg_level3),
               "You must specify what columns to join your map and data with.")
})
