
testthat::skip_if_not_installed("writexl")

# =============================================================================
# HELPERS
# =============================================================================

make_excel_file <- function(sheets) {

  path <- tempfile(fileext = ".xlsx")

  writexl::write_xlsx(
    sheets,
    path
  )

  path
}

# =============================================================================
# BASIC CONTRACT
# =============================================================================

test_that("returns one-row tibble with expected schema", {

  path <- tempfile(fileext = ".txt")

  writeLines(
    "fingerprint test",
    path
  )

  result <- file_fingerprint(path)

  expect_s3_class(result, "tbl_df")

  expect_equal(
    nrow(result),
    1
  )

  expect_named(
    result,
    c(
      "file_name",
      "file_path",
      "size_bytes",
      "mtime",
      "hash",
      "timestamp",
      "sheet"
    )
  )

  expect_true(
    file.exists(result$file_path)
  )

  expect_type(
    result$hash,
    "character"
  )

  expect_equal(
    length(result$hash),
    1
  )
})

# =============================================================================
# HASH ALGORITHMS
# =============================================================================

test_that("different algorithms produce expected hash lengths", {

  path <- tempfile(fileext = ".txt")

  writeLines(
    "hash validation",
    path
  )

  sha256 <- file_fingerprint(
    path,
    algo = "sha256"
  )

  md5 <- file_fingerprint(
    path,
    algo = "md5"
  )

  expect_equal(
    nchar(sha256$hash),
    64
  )

  expect_equal(
    nchar(md5$hash),
    32
  )

  expect_false(
    identical(
      sha256$hash,
      md5$hash
    )
  )
})

# =============================================================================
# DETERMINISM
# =============================================================================

test_that("hash is deterministic for unchanged files", {

  path <- tempfile(fileext = ".txt")

  writeLines(
    "stable content",
    path
  )

  r1 <- file_fingerprint(path)
  r2 <- file_fingerprint(path)

  expect_equal(
    r1$hash,
    r2$hash
  )

  expect_equal(
    r1$size_bytes,
    r2$size_bytes
  )
})

test_that("hash changes when file contents change", {

  path <- tempfile(fileext = ".txt")

  writeLines(
    "version_1",
    path
  )

  h1 <- file_fingerprint(path)$hash

  writeLines(
    "version_2",
    path
  )

  h2 <- file_fingerprint(path)$hash

  expect_false(
    identical(h1, h2)
  )
})

# =============================================================================
# FILE METADATA
# =============================================================================

test_that("file metadata is correctly populated", {

  path <- tempfile(fileext = ".txt")

  writeLines(
    c("a", "b", "c"),
    path
  )

  result <- file_fingerprint(path)

  expect_equal(
    result$file_name,
    basename(path)
  )

  expect_gt(
    result$size_bytes,
    0
  )

  expect_s3_class(
    result$mtime,
    "POSIXct"
  )

  expect_s3_class(
    result$timestamp,
    "POSIXct"
  )
})

# =============================================================================
# PATH HANDLING
# =============================================================================

test_that("full_path parameter behaves correctly", {

  path <- tempfile(fileext = ".txt")

  writeLines(
    "path test",
    path
  )

  full <- file_fingerprint(
    path,
    full_path = TRUE
  )

  relative <- file_fingerprint(
    path,
    full_path = FALSE
  )

  expect_equal(
    full$file_path,
    normalizePath(path, winslash = "/")
  )

  expect_equal(
    relative$file_path,
    path
  )
})

# =============================================================================
# INPUT VALIDATION
# =============================================================================

test_that("fails safely on missing files", {

  expect_error(
    file_fingerprint("missing_file.xlsx"),
    "File not found"
  )
})

test_that("fails safely on directories", {

  expect_error(
    file_fingerprint(tempdir()),
    "`path` must be a file"
  )
})

# =============================================================================
# EXCEL SHEET FINGERPRINTING
# =============================================================================

test_that("fingerprints Excel sheets correctly", {

  path <- make_excel_file(list(
    "PolicyData" = data.frame(
      policy_id = 1:3,
      premium = c(100, 200, 300)
    )
  ))

  result <- file_fingerprint(
    path,
    sheet = "PolicyData"
  )

  expect_s3_class(
    result,
    "tbl_df"
  )

  expect_equal(
    result$sheet,
    "PolicyData"
  )

  expect_false(
    is.na(result$hash)
  )
})

test_that("sheet index and sheet name produce same hash", {

  path <- make_excel_file(list(
    "Claims" = data.frame(
      claim_id = 1:5,
      amount = runif(5)
    )
  ))

  by_name <- file_fingerprint(
    path,
    sheet = "Claims"
  )

  by_index <- file_fingerprint(
    path,
    sheet = 1
  )

  expect_equal(
    by_name$hash,
    by_index$hash
  )
})

# =============================================================================
# COLUMN ORDER NORMALIZATION
# =============================================================================

test_that("ignore_column_order behaves correctly", {

  df1 <- data.frame(
    A = 1:3,
    B = letters[1:3],
    C = c(TRUE, FALSE, TRUE)
  )

  df2 <- df1[, c("C", "A", "B")]

  path1 <- make_excel_file(list(
    Sheet1 = df1
  ))

  path2 <- make_excel_file(list(
    Sheet1 = df2
  ))

  strict_1 <- file_fingerprint(
    path1,
    sheet = 1,
    ignore_column_order = FALSE
  )

  strict_2 <- file_fingerprint(
    path2,
    sheet = 1,
    ignore_column_order = FALSE
  )

  normalized_1 <- file_fingerprint(
    path1,
    sheet = 1,
    ignore_column_order = TRUE
  )

  normalized_2 <- file_fingerprint(
    path2,
    sheet = 1,
    ignore_column_order = TRUE
  )

  expect_false(
    identical(strict_1$hash, strict_2$hash)
  )

  expect_equal(
    normalized_1$hash,
    normalized_2$hash
  )
})

# =============================================================================
# MISSING VALUES
# =============================================================================

test_that("NA values are handled deterministically", {

  path <- make_excel_file(list(
    Sheet1 = data.frame(
      a = c(1, NA, 3),
      b = c("x", NA, "z")
    )
  ))

  r1 <- file_fingerprint(
    path,
    sheet = 1
  )

  r2 <- file_fingerprint(
    path,
    sheet = 1
  )

  expect_equal(
    r1$hash,
    r2$hash
  )
})

# =============================================================================
# EMPTY STRUCTURES
# =============================================================================

test_that("handles empty files safely", {

  path <- tempfile(fileext = ".txt")

  file.create(path)

  result <- file_fingerprint(path)

  expect_true(
    nchar(result$hash) > 0
  )
})

test_that("handles empty Excel sheets", {

  path <- make_excel_file(list(
    EmptySheet = data.frame()
  ))

  expect_no_error({

    result <- file_fingerprint(
      path,
      sheet = 1
    )

  })

  expect_true(
    nchar(result$hash) > 0
  )
})

# =============================================================================
# INSURANCE-GRADE EDGE CASES
# =============================================================================

test_that("handles very wide actuarial tables", {

  wide_df <- as.data.frame(
    matrix(
      runif(5000),
      nrow = 10
    )
  )

  names(wide_df) <- paste0(
    "feature_",
    seq_len(ncol(wide_df))
  )

  path <- make_excel_file(list(
    PricingFactors = wide_df
  ))

  expect_no_error({

    result <- file_fingerprint(
      path,
      sheet = 1
    )

  })

  expect_equal(
    nrow(result),
    1
  )
})

test_that("handles very large text payloads", {

  path <- tempfile(fileext = ".txt")

  writeLines(
    rep("insurance_reserving_pipeline", 100000),
    path
  )

  expect_no_error({

    result <- file_fingerprint(path)

  })

  expect_equal(
    nrow(result),
    1
  )
})

# =============================================================================
# FUZZ TESTING
# =============================================================================

test_that("handles unusual unicode safely", {

  path <- make_excel_file(list(
    "测试" = data.frame(
      "São Paulo" = c("ação", "café"),
      "Δοκιμή" = c("α", "β"),
      check.names = FALSE
    )
  ))

  expect_no_error({

    result <- file_fingerprint(
      path,
      sheet = 1
    )

  })

  expect_true(
    nchar(result$hash) > 0
  )
})

# =============================================================================
# CORRUPTION TESTING
# =============================================================================

test_that("fails safely on corrupted xlsx file", {

  path <- tempfile(fileext = ".xlsx")

  writeLines(
    "this is not a real excel file",
    path
  )

  expect_error(
    file_fingerprint(
      path,
      sheet = 1
    )
  )
})

test_that("fails safely on truncated xlsx structure", {

  path <- tempfile(fileext = ".xlsx")

  writeBin(
    as.raw(c(0x50, 0x4b, 0x03, 0x04)),
    path
  )

  expect_error(
    file_fingerprint(
      path,
      sheet = 1
    )
  )
})

# =============================================================================
# CROSS-FUNCTION CONSISTENCY
# =============================================================================

test_that("snapshot_excel and file_fingerprint remain consistent", {

  path <- make_excel_file(list(
    Sheet1 = data.frame(
      x = 1:5,
      y = letters[1:5]
    )
  ))

  snapshot <- snapshot_excel(path)

  fp <- file_fingerprint(
    path,
    sheet = "Sheet1"
  )

  expect_equal(
    unname(snapshot$hash[1]),
    unname(fp$hash)
  )
})

# =============================================================================
# DEPENDENCIES
# =============================================================================

test_that("required dependencies are available", {

  expect_true(
    requireNamespace("digest", quietly = TRUE)
  )

  expect_true(
    requireNamespace("tibble", quietly = TRUE)
  )

  expect_true(
    requireNamespace("readxl", quietly = TRUE)
  )
})
