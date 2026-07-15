# =============================================================================
# TEST FILE: snapshot_excel
# =============================================================================

testthat::skip_if_not_installed("writexl")
testthat::skip_if_not_installed("readxl")
testthat::skip_if_not_installed("tibble")

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

# =============================================================================
# CORE CONTRACT
# =============================================================================
test_that("snapshot_excel returns expected schema", {
  path <- make_excel_file(list(
    Policies = data.frame(
      id = 1:10,
      premium = runif(10)
    )
  ))

  result <- snapshot_excel(path)

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("sheet", "n_rows", "n_cols", "hash", "timestamp"))
  expect_equal(nrow(result), 1)
  expect_equal(result$n_rows, 10)
  expect_equal(result$n_cols, 2)
  expect_type(result$hash, "character")
  expect_s3_class(result$timestamp, "POSIXct")
})

# =============================================================================
# MULTI-SHEET CONTRACT
# =============================================================================
test_that("handles multiple sheets correctly", {
  path <- make_excel_file(list(
    Clientes = data.frame(id = 1:5, nome = letters[1:5]),
    Vendas   = data.frame(valor = runif(5), data = Sys.Date() + 1:5),
    Info     = data.frame(status = "OK")
  ))

  result <- snapshot_excel(path)

  expect_equal(nrow(result), 3)
  expect_equal(result$n_rows, c(5, 5, 1))
  expect_equal(result$n_cols, c(2, 2, 1))
})

# =============================================================================
# DETERMINISM & HASHING
# =============================================================================
test_that("snapshot hashes are deterministic", {
  path <- make_excel_file(list(
    Sheet1 = data.frame(x = 1:10, y = letters[1:10])
  ))

  r1 <- snapshot_excel(path)
  r2 <- snapshot_excel(path)
  expect_equal(r1$hash, r2$hash)
})

test_that("hash changes when sheet contents change", {
  path <- tempfile(fileext = ".xlsx")
  df1 <- data.frame(x = 1:5)
  writexl::write_xlsx(list(Sheet1 = df1), path)

  h1 <- snapshot_excel(path)$hash

  df2 <- data.frame(x = c(1:4, 999))
  writexl::write_xlsx(list(Sheet1 = df2), path)

  h2 <- snapshot_excel(path)$hash

  expect_false(identical(h1, h2))
})

# =============================================================================
# SHEET FILTERING
# =============================================================================
test_that("sheet filtering works correctly", {
  path <- make_excel_file(list(
    Sheet1 = mtcars,
    Sheet2 = iris,
    Sheet3 = cars
  ))

  result_all    <- snapshot_excel(path)
  result_subset <- snapshot_excel(path, sheets = c("Sheet1", "Sheet3"))

  expect_equal(nrow(result_all), 3)
  expect_equal(nrow(result_subset), 2)
  expect_equal(result_subset$sheet, c("Sheet1", "Sheet3"))
})

test_that("numeric sheet selection works", {
  path <- make_excel_file(list(A = mtcars, B = iris, C = cars))
  result <- snapshot_excel(path, sheets = c(1, 3))
  expect_equal(result$sheet, c("A", "C"))
})

# =============================================================================
# COLUMN ORDER NORMALIZATION
# =============================================================================
test_that("ignore_column_order behaves correctly", {
  df1 <- data.frame(A = 1:5, B = letters[1:5], C = runif(5))
  df2 <- df1[, c("C", "A", "B")]

  path1 <- make_excel_file(list(Sheet1 = df1))
  path2 <- make_excel_file(list(Sheet1 = df2))

  strict_1 <- snapshot_excel(path1, ignore_column_order = FALSE)
  strict_2 <- snapshot_excel(path2, ignore_column_order = FALSE)
  normalized_1 <- snapshot_excel(path1, ignore_column_order = TRUE)
  normalized_2 <- snapshot_excel(path2, ignore_column_order = TRUE)

  expect_false(identical(strict_1$hash, strict_2$hash))
  expect_equal(normalized_1$hash, normalized_2$hash)
})

# =============================================================================
# EMPTY & EDGE CASES
# =============================================================================
test_that("handles empty sheets safely", {

  path <- make_excel_file(

    list(

      EmptySheet = data.frame()

    )

  )

  result <- snapshot_excel(path)

  expect_equal(result$n_rows, 0)
  expect_equal(result$n_cols, 0)

})

# =============================================================================
# INPUT VALIDATION
# =============================================================================
test_that("fails safely on missing files", {
  expect_error(
    snapshot_excel("missing.xlsx"),
    "File does not exist"
  )
})

test_that("fails on unsupported Excel extension", {

  path <- tempfile(fileext = ".txt")
  writeLines("not excel", path)

  expect_error(
    snapshot_excel(path),
    "Unsupported Excel file extension"
  )

})

test_that("fails when requested sheets do not exist", {

  path <- make_excel_file(

    list(

      A = data.frame(x = 1)

    )

  )

  expect_error(

    snapshot_excel(

      path,

      sheets = "NON_EXISTENT"

    ),

    "Requested sheets not found"

  )

})

# =============================================================================
# CORRUPTION TESTING
# =============================================================================
test_that("fails on corrupted Excel files", {

  path <- make_corrupt_excel_file()

  expect_error(

    snapshot_excel(path),

    "Unable to read workbook structure"

  )

})

# =============================================================================
# CROSS-FUNCTION CONSISTENCY
# =============================================================================
test_that("snapshot_excel is consistent with file_fingerprint", {
  path <- make_excel_file(list(
    Sheet1 = data.frame(x = 1:5, y = letters[1:5])
  ))

  snapshot <- snapshot_excel(path)
  fp <- file_fingerprint(path, sheet = "Sheet1")

  expect_equal(unname(snapshot$hash[1]), unname(fp$hash))
})

# =============================================================================
# PERFORMANCE / INSURANCE SCALE
# =============================================================================
test_that("handles many sheets efficiently", {
  sheets <- setNames(
    replicate(25, data.frame(x = 1:50, y = runif(50)), simplify = FALSE),
    paste0("Sheet_", 1:25)
  )

  path <- make_excel_file(sheets)
  result <- snapshot_excel(path)
  expect_equal(nrow(result), 25)
})

test_that("handles wide tables", {
  wide_df <- as.data.frame(matrix(runif(5000), nrow = 100))
  names(wide_df) <- paste0("feature_", seq_len(ncol(wide_df)))

  path <- make_excel_file(list(PricingFactors = wide_df))

  result <- snapshot_excel(path)
  expect_equal(result$n_rows, 100)
})

# =============================================================================
# UNICODE & FUZZ TESTING
# =============================================================================
test_that("handles unicode sheet and column names", {
  path <- make_excel_file(list(
    "São Paulo" = data.frame(x = 1:3),
    "测试" = data.frame(y = letters[1:3]),
    "Δοκιμή" = data.frame(z = 1:3)
  ))

  result <- snapshot_excel(path)
  expect_equal(nrow(result), 3)
})
