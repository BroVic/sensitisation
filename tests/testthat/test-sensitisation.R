# test-sensitisation.R

context("Interactive display")

test_that("Improper data input is detected", {
  expect_error(display_data(42))
  expect_error(display_data())
  expect_error(display_data(TRUE))
  expect_error(display_data("random string"))
  expect_error(display_data("fakefile.fake"))
  expect_error(display_data("close-to-actual.csb"))
  expect_error(display_data("genuine-but-wrong.xlsx"))
})

test_that('imported data has the correct structure', {
  expect_true(file.exists('../test-dat.csv'), info = "Test file is missing")
  expect_error(chartApp('../test-dat.csv'))
})


context("Bar Chart Plotting")

test_that("open ended questions are filtered out", {
  expect_error(discard_comments(matrix(1:12)))
  expect_error(discard_comments(list(a = 1:10, b = LETTERS[1:10])))
})