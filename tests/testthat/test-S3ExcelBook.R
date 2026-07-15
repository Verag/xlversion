# =============================================================================
# TEST FILE: S3 Excel Book Class
# =============================================================================

testthat::skip_if_not_installed("testthat")

# =============================================================================
# HELPERS
# =============================================================================
make_sheet <- function(name = "Sheet1") {

  source <- tempfile(fileext = ".xlsx")

  writexl::write_xlsx(
    list(Test = data.frame(x = 1)),
    source
  )

  new_excel_sheet(
    data = data.frame(x = 1:3),
    sheet = name,
    source = source
  )

}


# =============================================================================
# CONSTRUCTOR
# =============================================================================

testthat::test_that(
  "new_excel_book creates a valid excel_book object",
  {

    sheets <- list(

      Sheet1 = make_sheet("Sheet1"),
      Sheet2 = make_sheet("Sheet2")

    )

    book <- new_excel_book(
      sheets = sheets
    )

    testthat::expect_s3_class(
      book,
      "excel_book"
    )

    testthat::expect_type(
      book$sheets,
      "list"
    )

    testthat::expect_length(
      book$sheets,
      2
    )

    testthat::expect_equal(
      names(book$sheets),
      c("Sheet1", "Sheet2")
    )

  }
)

testthat::test_that(
  "new_excel_book accepts an empty workbook",
  {

    book <- new_excel_book()

    testthat::expect_s3_class(
      book,
      "excel_book"
    )

    testthat::expect_length(
      book$sheets,
      0
    )

    testthat::expect_length(
      book$failed_sheets,
      0
    )

  }
)

# =============================================================================
# PRINT METHOD
# =============================================================================

testthat::test_that(
  "print.excel_book displays workbook summary",
  {

    book <- new_excel_book(

      sheets = list(

        Sales = make_sheet("Sales")

      )

    )

    output <- capture.output(
      print(book)
    )

    testthat::expect_true(
      any(grepl("<excel_book>", output))
    )

    testthat::expect_true(
      any(grepl("Sheets:", output))
    )

    testthat::expect_true(
      any(grepl("Sales", output))
    )

    testthat::expect_true(
      any(grepl("Failed:", output))
    )

  }
)

testthat::test_that(
  "print.excel_book handles empty workbooks",
  {

    book <- new_excel_book()

    output <- capture.output(
      print(book)
    )

    testthat::expect_true(
      any(grepl("<excel_book>", output))
    )

    testthat::expect_true(
      any(grepl("Sheets:", output))
    )

    testthat::expect_true(
      any(grepl("0", output))
    )

  }
)

# =============================================================================
# get_sheet
# =============================================================================

testthat::test_that(
  "get_sheet extracts a worksheet by name",
  {

    book <- new_excel_book(

      sheets = list(

        Clients = make_sheet("Clients")

      )

    )

    sheet <- get_sheet(
      book,
      "Clients"
    )

    testthat::expect_s3_class(
      sheet,
      "excel_sheet"
    )

    testthat::expect_equal(
      sheet$sheet,
      "Clients"
    )

  }
)

testthat::test_that(
  "get_sheet extracts a worksheet by numeric index",
  {

    book <- new_excel_book(

      sheets = list(

        A = make_sheet("A"),
        B = make_sheet("B")

      )

    )

    sheet <- get_sheet(
      book,
      2
    )

    testthat::expect_equal(
      sheet$sheet,
      "B"
    )

  }
)

testthat::test_that(
  "get_sheet warns when worksheet does not exist",
  {

    book <- new_excel_book(

      sheets = list(

        Sales = make_sheet("Sales")

      )

    )

    testthat::expect_warning(

      result <- get_sheet(
        book,
        "MissingSheet"
      ),

      "Sheet not found"

    )

    testthat::expect_null(
      result
    )

  }
)

testthat::test_that(
  "get_sheet rejects invalid indices",
  {

    book <- new_excel_book(

      sheets = list(

        Sales = make_sheet("Sales")

      )

    )

    testthat::expect_error(

      get_sheet(
        book,
        2
      ),

      "Sheet index out of bounds"

    )

  }
)

testthat::test_that(
  "get_sheet rejects non excel_book objects",
  {

    testthat::expect_error(

      get_sheet(
        list(),
        "Sheet1"
      ),

      "`book` must be an excel_book object"

    )

  }
)

# =============================================================================
# sheet_names
# =============================================================================

testthat::test_that(
  "sheet_names returns worksheet names",
  {

    book <- new_excel_book(

      sheets = list(

        A = make_sheet("A"),
        B = make_sheet("B")

      )

    )

    testthat::expect_equal(

      sheet_names(book),

      c("A", "B")

    )

  }
)

testthat::test_that(
  "sheet_names returns character(0) for empty workbook",
  {

    book <- new_excel_book()

    testthat::expect_identical(

      sheet_names(book),

      character(0)

    )

  }
)

testthat::test_that(
  "sheet_names rejects invalid objects",
  {

    testthat::expect_error(

      sheet_names(list()),

      "`x` must be an excel_book object"

    )

  }
)
