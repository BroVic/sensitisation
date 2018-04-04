# test-sensitisation.R

context("Check interactive display")

test_that("Improper data input is detected", {
  expect_error(display_charts(42))
  expect_error(display_charts())
  expect_error(display_charts(TRUE))
  expect_error(display_charts("random string"))
  expect_error(display_charts("fakefile.fake"))
  expect_error(display_charts("close-to-actual.csb"))
  expect_error(display_charts("genuine-but-wrong.xlsx"))
})