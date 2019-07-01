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

test_that("removes empty features from data if user specifies", {
  test_data <- data.frame(region=c("BZN", "BZE", "BZC"), n=c(10,20,30))
  map <- make_thematic_map(test_data, n, .map=tdwg_level3, join_by=c("LEVEL3_COD"="region"),
                           remove_na=TRUE)

  mapped_data <- map$layers[[1]]$data

  expect_false(any(is.na(mapped_data$n)))
})

test_that("does not remove empty features from data if user specifies", {
  test_data <- data.frame(region=c("BZN", "BZE", "BZC"), n=c(10,20,30))
  map <- make_thematic_map(test_data, n, .map=tdwg_level3, join_by=c("LEVEL3_COD"="region"),
                           remove_na=FALSE)

  mapped_data <- map$layers[[1]]$data

  expect_true(any(is.na(mapped_data$n)))
})
