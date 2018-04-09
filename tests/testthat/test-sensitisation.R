# test-sensitisation.R

context("Check interactive display")

test_that("Improper data input is detected", {
  expect_error(display_data(42))
  expect_error(display_data())
  expect_error(display_data(TRUE))
  expect_error(display_data("random string"))
  expect_error(display_data("fakefile.fake"))
  expect_error(display_data("close-to-actual.csb"))
  expect_error(display_data("genuine-but-wrong.xlsx"))
})