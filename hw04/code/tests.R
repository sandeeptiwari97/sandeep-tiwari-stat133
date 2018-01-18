# test script
library(testthat)

#source in functions to be tested
source('functions.R')

sink('../output/test-reporter.txt')
test_file('tests.R')
sink()

context("Remove missing values from vector")
test_that("remove_missing returns vector with all NA values removed", {
  expect_equal(remove_missing(c(1, 4, 7, NA, 10)), c(1, 4, 7, 10))
  expect_equal(remove_missing(c(1, NA)), c(1))
  expect_equal(remove_missing(c(NA)), logical(0))
  expect_equal(remove_missing(c(1, 2, 3, 4)), c(1, 2, 3, 4))
})

context("Minimum of numeric vector")
test_that("get_minimum returns minimum of numeric vector", {
  expect_equal(get_minimum(c(1, 2, 3, 4)), 1)
  expect_equal(get_minimum(c('a', 1)), "ERROR: non-numeric argument")
  expect_equal(get_minimum(c(1, 4, 7, NA, 10), na.rm = TRUE), 1)
  expect_equal(get_minimum(c(NA), na.rm = TRUE), "ERROR: non-numeric argument")
})

context("Maximum of numeric vector")
test_that("get_maximum returns maximum of numeric vector", {
  expect_equal(get_maximum(c(1, 2, 3, 4)), 4)
  expect_equal(get_maximum(c('a', 1)), "ERROR: non-numeric argument")
  expect_equal(get_maximum(c(1, 4, 7, NA, 10), na.rm = TRUE), 10)
  expect_equal(get_maximum(c(NA), na.rm = TRUE), "ERROR: non-numeric argument")
})

context("Get range of numeric vector")
test_that("get_range returns range of numeric vector", {
  expect_equal(get_range(c(1, 2, 3, 4)), 3)
  expect_equal(get_range(c('a', 1)), "ERROR: non-numeric argument")
  expect_equal(get_range(c(1, 4, 7, NA, 10), na.rm = TRUE), 9)
  expect_equal(get_range(c(NA), na.rm = TRUE), "ERROR: non-numeric argument")
})

context("Get 10th percentile of numeric vector")
test_that("get_percentile10 returns 10th percentile of numeric vector", {
  expect_equal(get_percentile10(1:100), 10.9)
  expect_equal(get_percentile10(c(1, 4, 7, NA, 10), na.rm = TRUE), 1.9)
  expect_equal(get_percentile10(c('a', 1)), "ERROR: non-numeric argument")
  expect_equal(get_percentile10(c(NA), na.rm = TRUE), "ERROR: non-numeric argument")
})

context("Get 90th percentile of numeric vector")
test_that("get_percentile90 returns 90th percentile of numeric vector", {
  expect_equal(get_percentile90(c(1, 4, 7, NA, 10), na.rm = TRUE), 9.1)
  expect_equal(get_percentile90(c('a', 1)), "ERROR: non-numeric argument")
  expect_equal(get_percentile90(c(NA), na.rm = TRUE), "ERROR: non-numeric argument")
  expect_equal(get_percentile90(c(1:10)), 9.1)
})

context("Get median of numeric vector")
test_that("get_median returns median of numeric vector", {
  expect_equal(get_median(c(1, 4, 7, NA, 10)), 5.5)
  expect_equal(get_median(c('a', 1)), "ERROR: non-numeric argument")
  expect_equal(get_median(c(2, NA), na.rm = TRUE), 2)
  expect_equal(get_median(c(1, 1, 1)), 1)
})

context("Get average of numeric vector")
test_that("get_average returns mean of numeric vector", {
  expect_equal(get_average(c(1, 4, 7, NA, 10), na.rm = TRUE), 5.5)
  expect_equal(get_average(c(1, 1, 1)), 1)
  expect_equal(get_average(c(NA), na.rm = TRUE), "ERROR: non-numeric argument")
  expect_equal(get_average(c("a")), "ERROR: non-numeric argument")
})

context("Get standard deviation of numeric vector")
test_that("get_stdev returns standard deviation of numeric vector", {
  expect_equal(get_stdev(c(1, 4, 7, NA, 10), na.rm = TRUE), 3.8729834)
  expect_equal(get_stdev(c(1, 1, 1)), 0)
  expect_equal(get_stdev(c(NA), na.rm = TRUE), "ERROR: non-numeric argument")
  expect_equal(get_stdev(c("a")), "ERROR: non-numeric argument")
})

context("Get first quartile of numeric vector")
test_that("get_quartile1 returns first quartile of numeric vector", {
  expect_equal(get_quartile1(c(1, 4, 7, NA, 10), na.rm = TRUE), 3.25)
  expect_equal(get_quartile1(c(1, NA), na.rm = TRUE), 1)
  expect_equal(get_quartile1(c(NA), na.rm = TRUE), "ERROR: non-numeric argument")
  expect_equal(get_quartile1(c("a")), "ERROR: non-numeric argument")
})

context("Get third quartile of numeric vector")
test_that("get_quartile3 returns first quartile of numeric vector", {
  expect_equal(get_quartile3(c(1, 4, 7, NA, 10), na.rm = TRUE), 7.75)
  expect_equal(get_quartile3(c(1, NA), na.rm = TRUE), 1)
  expect_equal(get_quartile3(c(NA), na.rm = TRUE), "ERROR: non-numeric argument")
  expect_equal(get_quartile3(c("a")), "ERROR: non-numeric argument")
})

context("Count number of missing values in numeric vector")
test_that("count_missing returns number of NA values", {
  expect_equal(count_missing(c(1, 4, 7, NA, 10)), 1)
  expect_equal(count_missing(c(1, NA)), 1)
  expect_equal(count_missing(c(NA, NA, NA)), 3)
  expect_equal(count_missing(c("a")), 0)
})

context("Print summary statistics of numeric vector")
test_that("summary_statistics prints list of statistiscs", {
  expect_equal(summary_statistics(c(1, 4, 7, NA, 10))$minimum, 1)
  expect_equal(summary_statistics(c(1, 4, 7, NA, 10))$percent90, 9.1)
  expect_equal(summary_statistics(c(1, 4, 7, NA, 10))$range, 9)
  expect_equal(summary_statistics(c(1, 4, 7, NA, 10))$missing, 1)
})

context("Rescale numeric vector to scale from 0 to 100")
test_that("rescale100 returns vector of rescaled values", {
  expect_equal(rescale100(c(18, 15, 16, 4, 17, 9), xmin = 0, xmax = 20), 
               c(90, 75, 80, 20, 85, 45))
  expect_equal(rescale100(c(1, 2, 3, 4), xmin = 1, xmax = 5), c(0, 25, 50, 75))
  expect_equal(rescale100(c(1, 2, 3, 4), xmin = 0, xmax = 10), 
               c(10, 20, 30, 40))
  expect_equal(rescale100(c(0), xmin = 0, xmax = 0), NaN)
})

context("Drop lowest value in numeric vector")
test_that("drop_lowest returns vector with lowest value deleted", {
  expect_equal(drop_lowest(c(18, 15, 16, 4, 17, 9)), c(18, 15, 16, 17, 9))
  expect_equal(drop_lowest(c(1, 1, 2)), c(1, 2))
  expect_equal(drop_lowest(c(3, 1, 2)), c(3, 2))
  expect_equal(drop_lowest(c(3, 2, 2, 6)), c(3, 2, 6))
})

context("Returns average of homework scores")
test_that("score_homework returns average of homework scores", {
  expect_equal(score_homework(c(100, 80, 30, 70, 75, 85), drop = TRUE), 82)
  expect_equal(score_homework(c(100, 80, 30, 70, 75, 85)), 73.333333)
  expect_equal(score_homework(c(10, 10, 100), drop = TRUE), 55)
  expect_equal(score_homework(c(10, 10, 100)), 40)
})

context("Returns average of quiz scores")
test_that("score_quiz returns average of quiz scores", {
  expect_equal(score_quiz(c(100, 80, 70, 0), drop = TRUE), 83.333333)
  expect_equal(score_quiz(c(100, 80, 70, 0)), 62.5)
  expect_equal(score_quiz(c(10, 10, 100), drop = TRUE), 55)
  expect_equal(score_quiz(c(10, 10, 100)), 40)
})

context("Returns grade based on lab attendance score")
test_that("score_lab returns grade based on lab attendace score", {
  expect_equal(score_lab(12), 100)
  expect_equal(score_lab(11), 100)
  expect_equal(score_lab(10), 80)
  expect_equal(score_lab(9), 60)
  expect_equal(score_lab(8), 40)
  expect_equal(score_lab(7), 20)
  expect_equal(score_lab(6), 0)
})
