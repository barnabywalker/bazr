test_that("deduplication by one column works", {
  test_df <- data.frame(x=c("a", "b", "c", "a", "a"),
                        y=c(1, 2, 3, 10, 1))

  deduplicated_df <- deduplicate_by(test_df, x)

  expect_equal(nrow(deduplicated_df), 3)
})

test_that("deduplication by more than one column works", {
  test_df <- data.frame(x=c("a", "b", "c", "a", "a"),
                        y=c(1, 2, 3, 10, 1))

  deduplicated_df <- deduplicate_by(test_df, x, y)

  expect_equal(nrow(deduplicated_df), 4)
})
