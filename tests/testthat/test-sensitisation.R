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
    diff.long.name = rep(letters, 10)
  )

tdf <- .prepareDataframe(tdf)

## Tests proper
# ============================
context('Data importation')

csvTest <- '../test-dat.csv'
rdsTest <- '../test-dat.rds'
sqlTest <- '../test-dat.db'

test_that('imported data has the correct structure', {
  expect_true(file.exists(csvTest))
  # expect_error(chartApp('../test-dat.csv'))
})

test_that('various formats are identified', {
  expect_error(readData("random"))
  expect_error(readData("fake.fake"))
  expect_error(readData("close.csb"))
  expect_error(readData("noexist.xlsx"))
  expect_error(readSQLite(csvTest))
  expect_warning(readData(c(csvTest, "noexist.csv")),
                          'Expect character vector of length == 1L;')
  expect_is(readData(csvTest), 'data.frame')
  expect_is(readData(rdsTest), 'data.frame')
  # TODO: expect_is(readData(sqlTest), 'data.frame')
  expect_is(readSQLite(sqlTest, 'iris'), 'data.frame')
})

# ============================
context("Interactive display")

test_that("Improper data input is detected", {
  expect_error(chartApp())
  expect_error(chartApp(testMatrix))
  expect_error(display_data())
  expect_error(display_data(42))
  expect_error(display_data(TRUE))
})

# ---


# ===========================
context("Bar Chart Plotting")

test_that("open ended questions are filtered out", {
  msg <- 'must be an object of class \'data.frame\''
  
  expect_error(keepOnlyFactors(matrix(1:12)), msg)
  expect_error(keepOnlyFactors(list(a = 1:10, b = LETTERS[1:10])), msg)
})

test_that("ggplotObj can be created", {
  expect_error(drawBarChart(tdf, 999))
  expect_output(drawBarChart(tdf, colnames(tdf)[1]))
})

## Also create a matrix for testing some error checks
testMatrix <- matrix(1:4, nrow = 2)

test_that("Multiple plots are printed via graphics device", {
  expect_error(show_all_barcharts(42))
  expect_error(show_all_barcharts())
  expect_error(show_all_barcharts(data = testMatrix))
})

test_that('bars can be sorted', {
  val <- setBarCategoryOrder(tdf, 'when.cleans')
  
  expect_error(setBarCategoryOrder())
  expect_error(setBarCategoryOrder(42))
  expect_error(setBarCategoryOrder(42, 999))
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
  expect_is(tdf[[3]], "character")
  expect_true(is.ordered(tdf[[1]]))
  expect_true(is.ordered(tdf[[2]]))
  expect_false(is.ordered(tdf[[3]]))
})
# ----------------------------------------------