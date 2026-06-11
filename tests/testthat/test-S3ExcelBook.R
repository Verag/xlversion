# =============================================================================
# TEST FILE: S3 Excel Book Class
# =============================================================================

testthat::skip_if_not_installed("testthat")

# =============================================================================
# TESTS FOR new_excel_book AND print.excel_book
# =============================================================================

testthat::test_that("new_excel_book creates valid S3 object", {
  sheets <- list(
    "Sheet1" = new_excel_sheet(data.frame(a = 1:3), "Sheet1"),
    "Sheet2" = new_excel_sheet(data.frame(b = 4:6), "Sheet2")
  )

  book <- new_excel_book(sheets)

  testthat::expect_s3_class(book, "excel_book")
  testthat::expect_type(book$sheets, "list")
  testthat::expect_length(book$sheets, 2)
})

testthat::test_that("new_excel_book handles empty list gracefully", {
  book <- new_excel_book(list())

  testthat::expect_s3_class(book, "excel_book")
  testthat::expect_length(book$sheets, 0)
})

testthat::test_that("print.excel_book shows basic information", {
  sheets <- list(
    "Sales" = new_excel_sheet(data.frame(sales = 1:5), "Sales")
  )
  book <- new_excel_book(sheets)

  # Capture output safely
  output <- capture.output(print(book))

  testthat::expect_true(any(grepl("<excel_book>", output)))
  testthat::expect_true(any(grepl("Total sheets", output)))
  testthat::expect_true(any(grepl("Sales", output)))
})

testthat::test_that("print.excel_book handles empty book", {
  book <- new_excel_book(list())
  output <- capture.output(print(book))

  testthat::expect_true(any(grepl("<excel_book>", output)))
  testthat::expect_true(any(grepl("Total sheets : 0", output)))
  testthat::expect_true(any(grepl("none", output, ignore.case = TRUE)))
})

testthat::test_that("get_sheet extracts sheet correctly", {
  book <- new_excel_book(list(
    "Clients" = new_excel_sheet(data.frame(id = 1:10), "Clients")
  ))

  sheet <- get_sheet(book, "Clients")
  testthat::expect_s3_class(sheet, "excel_sheet")
  testthat::expect_equal(sheet$sheet, "Clients")
})

testthat::test_that("get_sheet warns when sheet does not exist", {
  book <- new_excel_book(list(
    "Sales" = new_excel_sheet(data.frame(x = 1), "Sales")
  ))

  testthat::expect_warning(
    get_sheet(book, "MissingSheet"),
    "not found"
  )
})
