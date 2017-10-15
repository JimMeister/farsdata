library(testthat)
context("make_filename")

test_that("make_filename with single inputs", {
  expect_equal(make_filename(2014),"accident_2014.csv.bz2")
  expect_equal(make_filename("2014"),"accident_2014.csv.bz2")
})

test_that("make_filename errors with non-numeric string", {
  throws_error(make_filename("two thousand"))
})
