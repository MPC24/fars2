library(dplyr)


test_that("fars_read() works correctly", {
  yrs <- c(2013, 2014, 2015)
  smy <- fars_read_years(yrs)
  dms <- lapply(smy, dim)

  expect_equal(dms[[1]][1], 30202)
  expect_equal(dms[[1]][2], 2)
  expect_equal(dms[[2]][1], 30056)
  expect_equal(dms[[2]][2], 2)
  expect_equal(dms[[3]][1], 32166)
  expect_equal(dms[[3]][2], 2)
})

