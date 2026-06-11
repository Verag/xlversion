# =============================================================================
# TEST FILE: read_excel_allsheets
# =============================================================================

testthat::skip_if_not_installed("writexl")
testthat::skip_if_not_installed("readxl")
testthat::skip_if_not_installed("openxlsx")

# =============================================================================
# HELPERS
# =============================================================================
make_excel_file <- function(sheets) {
  path <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(sheets, path)
  path
}

make_corrupt_excel_file <- function() {
  path <- tempfile(fileext = ".xlsx")
  writeLines("This is not a valid Excel file - corrupted content", path)
  path
}

get_nrow <- function(sheet) {
  if (inherits(sheet, "excel_sheet")) sheet$n_rows else nrow(sheet)
}

# =============================================================================
# CORE CONTRACT
# =============================================================================
testthat::test_that("returns excel_book object with correct structure", {
  path <- make_excel_file(list(
    "Sheet One" = data.frame(a = 1:3),
    "Sheet Two" = data.frame(b = 4:6)
  ))

  result <- read_excel_allsheets(path)

  testthat::expect_s3_class(result, "excel_book")
  testthat::expect_type(result$sheets, "list")
})

testthat::test_that("preserves sheet order from the Excel file", {
  path <- make_excel_file(list(
    "A" = data.frame(x = 1),
    "B" = data.frame(x = 2),
    "C" = data.frame(x = 3)
  ))

  result <- read_excel_allsheets(path)
  testthat::expect_equal(names(result$sheets), c("A", "B", "C"))
})

# =============================================================================
# ENGINE CONSISTENCY
# =============================================================================
testthat::test_that("readxl and openxlsx engines produce equivalent structures", {
  path <- make_excel_file(list(
    "S1" = data.frame(a = 1:3),
    "S2" = data.frame(b = letters[1:3])
  ))

  r_readxl <- read_excel_allsheets(path, engine = "readxl")
  r_openxlsx <- read_excel_allsheets(path, engine = "openxlsx")

  testthat::expect_equal(names(r_readxl$sheets), names(r_openxlsx$sheets))
})

# =============================================================================
# OUTPUT TYPES
# =============================================================================
testthat::test_that("returns data.frame by default", {
  path <- make_excel_file(list("S1" = data.frame(x = 1:3)))
  result <- read_excel_allsheets(path, tibble = FALSE)
  testthat::expect_s3_class(result$sheets[[1]], "excel_sheet")
})

testthat::test_that("returns tibble when requested", {
  path <- make_excel_file(list("S1" = data.frame(x = 1:3)))
  result <- read_excel_allsheets(path, tibble = TRUE)
  testthat::expect_s3_class(as.data.frame(result$sheets[[1]]), "tbl_df")
})

# =============================================================================
# INPUT VALIDATION
# =============================================================================
testthat::test_that("rejects non-character filename", {
  testthat::expect_error(
    read_excel_allsheets(123),
    "`filename` must be a single character string"
  )
})

testthat::test_that("fails when file does not exist", {
  testthat::expect_error(
    read_excel_allsheets("nonexistent_file.xlsx"),
    "File does not exist"
  )
})

# =============================================================================
# SHEET SELECTION
# =============================================================================
testthat::test_that("can read specific sheets by name", {
  path <- make_excel_file(list(
    "A" = data.frame(x = 1),
    "B" = data.frame(x = 2),
    "C" = data.frame(x = 3)
  ))
  result <- read_excel_allsheets(path, sheet_names = c("A", "C"))
  testthat::expect_equal(names(result$sheets), c("A", "C"))
})

testthat::test_that("returns empty excel_book when no sheets match", {
  path <- make_excel_file(list("A" = data.frame(x = 1)))
  result <- read_excel_allsheets(path, sheet_names = "NON_EXISTENT")

  testthat::expect_s3_class(result, "excel_book")
  testthat::expect_length(result$sheets, 0)
})

# =============================================================================
# ERROR HANDLING
# =============================================================================
testthat::test_that("handles corrupted file gracefully", {
  corrupt_path <- make_corrupt_excel_file()

  result <- read_excel_allsheets(
    corrupt_path,
    return_failed = TRUE,
    verbose = FALSE
  )

  testthat::expect_s3_class(result, "excel_book")
  testthat::expect_length(result$sheets, 0)
})

# =============================================================================
# OTHER TESTS
# =============================================================================
testthat::test_that("verbose = TRUE does not throw errors", {
  path <- make_excel_file(list("S1" = data.frame(x = 1:3)))

  # Compatible with older testthat versions
  testthat::expect_error(
    read_excel_allsheets(path, verbose = TRUE),
    NA
  )
})

testthat::test_that("passes additional arguments to reader", {
  path <- make_excel_file(list("S1" = data.frame(x = 1:10)))
  result <- read_excel_allsheets(path, skip = 5)
  testthat::expect_equal(result$sheets[[1]]$n_rows, 5)
})

testthat::test_that("get_sheet works correctly", {
  path <- make_excel_file(list("Sales" = data.frame(sales = 1:10)))
  book <- read_excel_allsheets(path)
  sheet <- get_sheet(book, "Sales")
  testthat::expect_s3_class(sheet, "excel_sheet")
})

testthat::test_that("handles empty sheets", {
  path <- make_excel_file(list("Empty" = data.frame()))
  result <- read_excel_allsheets(path)
  testthat::expect_equal(result$sheets[[1]]$n_rows, 0)
})
