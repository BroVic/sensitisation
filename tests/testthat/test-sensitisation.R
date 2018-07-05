# test-sensitisation.

## Create test data frame
set.seed(123)
tdf <-
  data.frame(
    when.cleans = factor(sample(c(
      "Morning Only", "Afternoon", "Morning and Evening"
    ), 260, replace = TRUE)),
    freq.evacuates = factor(sample(
      c(
        "Daily",
        "At least twice a week",
        "Weekly",
        "Fortnightly",
        "Not sure"
      ),
      260,
      replace = TRUE
    )),
    Another.long.name = rep(letters, 10)
  )

tdf <- .prepareDataframe(tdf)

## Also create a matrix for testing some error checks
testMatrix <- matrix(1:4, nrow = 2)

## Tests proper
# ============================
context("Interactive display")

test_that("Improper data input is detected", {
  expect_error(chartApp())
  expect_error(chartApp(testMatrix))
  
  expect_error(display_data())
  expect_error(display_data(42))
  expect_error(display_data(TRUE))
  
  expect_error(readData("random string"))
  expect_error(readData("fakefile.fake"))
  expect_error(readData("close-to-actual.csb"))
  expect_error(readData("genuine-but-nonexistent.xlsx"))
})

test_that('imported data has the correct structure', {
  expect_true(file.exists('../test-dat.csv'))
  # expect_error(chartApp('../test-dat.csv'))
})
# ---


# ===========================
context("Bar Chart Plotting")

test_that("open ended questions are filtered out", {
  expect_error(discard_comments(matrix(1:12)))
  expect_error(discard_comments(list(a = 1:10, b = LETTERS[1:10])))
})

test_that("ggplotObj can be created", {
  expect_error(drawBarChart(tdf, 999))
  expect_output(drawBarChart(tdf, colnames(tdf)[1]))
})

test_that("Multiple plots are printed via graphics device", {
  expect_error(show_all_barcharts(42))
  expect_error(show_all_barcharts())
  expect_error(show_all_barcharts(data = testMatrix))
})

test_that('bars can be sorted', {
  val <- setBarCatOrder(tdf, 'when.cleans')
  
  expect_error(setBarCatOrder())
  expect_error(setBarCatOrder(42))
  expect_error(setBarCatOrder(42, 999))
  expect_is(val, 'factor')
  expect_type(val, 'integer')

})
# ---

# =================================
context("Data frame preprocessing")

test_that("Factors can become ordered", {
  expect_error(.prepareDataframe(1:10))
  expect_is(tdf, "data.frame")
  expect_is(tdf[[1]], "factor")
  expect_is(tdf[[2]], "factor")
  expect_is(tdf[[3]], "factor")
  expect_true(is.ordered(tdf[[1]]))
  expect_true(is.ordered(tdf[[2]]))
  expect_false(is.ordered(tdf[[3]]))
})
# ----------------------------------------------