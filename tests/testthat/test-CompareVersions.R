# tests/testthat/test-CompareVersions.R

testthat::skip_if_not_installed("writexl")

library(testthat)


# =============================================================================
# HELPERS
# =============================================================================

make_excel_file <- function(sheets) {

  path <- tempfile(
    fileext = ".xlsx"
  )

  writexl::write_xlsx(
    sheets,
    path
  )

  path
}



# =============================================================================
# CORE CONTRACT
# =============================================================================

test_that(
  "returns tibble with expected structure",
  {

    old_path <- make_excel_file(
      list(
        Sheet1 = data.frame(
          x = 1:3
        )
      )
    )


    new_path <- make_excel_file(
      list(
        Sheet1 = data.frame(
          x = 1:3
        )
      )
    )


    result <- compare_excel_versions(
      old_path,
      new_path,
      verbose = FALSE
    )


    expect_s3_class(
      result,
      "tbl_df"
    )


    required_cols <- c(

      "sheet",
      "status",
      "change_type",
      "changed",

      "rows_diff",
      "cols_diff",

      "n_rows_old",
      "n_rows_new",

      "n_cols_old",
      "n_cols_new",

      "hash_old",
      "hash_new"

    )


    expect_true(
      all(
        required_cols %in% names(result)
      )
    )

  })



# =============================================================================
# CONTENT CHANGE
# =============================================================================

test_that(
  "detects content changes correctly",
  {

    old_path <- make_excel_file(
      list(
        Data = data.frame(
          ID = 1:10,
          Value = 1:10
        )
      )
    )


    new_path <- make_excel_file(
      list(
        Data = data.frame(
          ID = 1:10,
          Value = c(
            1:9,
            999
          )
        )
      )
    )


    result <- compare_excel_versions(
      old_path,
      new_path,
      verbose = FALSE
    )


    expect_equal(
      nrow(result),
      1
    )


    expect_true(
      result$changed
    )


    expect_equal(
      result$status,
      "Content changed"
    )


    expect_equal(
      result$change_type,
      "data"
    )

  })



# =============================================================================
# STRUCTURE CHANGE
# =============================================================================

test_that(
  "detects structure changes correctly",
  {

    old_path <- make_excel_file(
      list(
        Data = data.frame(
          A = 1:5,
          B = 6:10
        )
      )
    )


    new_path <- make_excel_file(
      list(
        Data = data.frame(
          A = 1:5,
          B = 6:10,
          C = 11:15
        )
      )
    )


    result <- compare_excel_versions(
      old_path,
      new_path,
      verbose = FALSE
    )


    expect_true(
      result$changed
    )


    expect_equal(
      result$status,
      "Structure changed"
    )


    expect_equal(
      result$change_type,
      "structure"
    )


    expect_equal(
      result$cols_diff,
      1
    )

  })



# =============================================================================
# NEW SHEETS
# =============================================================================

test_that(
  "detects new sheets correctly",
  {

    old_path <- make_excel_file(
      list(
        A = data.frame(
          x = 1
        )
      )
    )


    new_path <- make_excel_file(
      list(
        A = data.frame(
          x = 1
        ),
        B = data.frame(
          y = 2
        )
      )
    )


    result <- compare_excel_versions(
      old_path,
      new_path,
      verbose = FALSE
    )


    expect_equal(
      sum(result$status == "New sheet"),
      1
    )

  })



# =============================================================================
# DELETED SHEETS
# =============================================================================

test_that(
  "detects deleted sheets correctly",
  {

    old_path <- make_excel_file(
      list(
        A = data.frame(
          x = 1
        ),
        B = data.frame(
          y = 2
        )
      )
    )


    new_path <- make_excel_file(
      list(
        A = data.frame(
          x = 1
        )
      )
    )


    result <- compare_excel_versions(
      old_path,
      new_path,
      verbose = FALSE
    )


    expect_equal(
      sum(result$status == "Deleted sheet"),
      1
    )

  })



# =============================================================================
# UNCHANGED
# =============================================================================

test_that(
  "detects unchanged files correctly",
  {

    old_path <- make_excel_file(
      list(
        Data = mtcars
      )
    )


    new_path <- make_excel_file(
      list(
        Data = mtcars
      )
    )


    result <- compare_excel_versions(
      old_path,
      new_path,
      verbose = FALSE
    )


    expect_false(
      any(result$changed)
    )


    expect_true(
      all(
        result$status == "Unchanged"
      )
    )

  })



# =============================================================================
# ONLY CHANGES
# =============================================================================

test_that(
  "only_changes filters unchanged sheets",
  {

    old_path <- make_excel_file(
      list(
        A = mtcars,
        B = iris
      )
    )


    new_path <- make_excel_file(
      list(
        A = mtcars,
        B = iris
      )
    )


    result <- compare_excel_versions(
      old_path,
      new_path,
      only_changes = TRUE,
      verbose = FALSE
    )


    expect_s3_class(
      result,
      "tbl_df"
    )


    expect_equal(
      nrow(result),
      0
    )

  })



# =============================================================================
# COLUMN ORDER
# =============================================================================

test_that(
  "ignore_column_order works correctly",
  {

    old_path <- make_excel_file(
      list(
        Data = data.frame(
          A = 1:5,
          B = letters[1:5]
        )
      )
    )


    new_path <- make_excel_file(
      list(
        Data = data.frame(
          B = letters[1:5],
          A = 1:5
        )
      )
    )


    strict_result <- compare_excel_versions(
      old_path,
      new_path,
      ignore_column_order = FALSE,
      verbose = FALSE
    )


    relaxed_result <- compare_excel_versions(
      old_path,
      new_path,
      ignore_column_order = TRUE,
      verbose = FALSE
    )


    expect_true(
      strict_result$changed
    )


    expect_false(
      relaxed_result$changed
    )

  })



# =============================================================================
# SHEET FILTERING
# =============================================================================

test_that(
  "specific sheet comparison works",
  {

    old_path <- make_excel_file(
      list(
        Summary = data.frame(
          x = 1:3
        ),
        Detail = iris
      )
    )


    new_path <- make_excel_file(
      list(
        Summary = data.frame(
          x = 1:5
        ),
        Detail = iris
      )
    )


    result <- compare_excel_versions(
      old_path,
      new_path,
      old_sheets = "Summary",
      new_sheets = "Summary",
      verbose = FALSE
    )


    expect_equal(
      nrow(result),
      1
    )


    expect_equal(
      result$sheet,
      "Summary"
    )


    expect_equal(
      result$status,
      "Structure changed"
    )

  })



# =============================================================================
# HASH ALGORITHMS
# =============================================================================

test_that(
  "supports multiple hash algorithms",
  {

    old_path <- make_excel_file(
      list(
        Data = mtcars
      )
    )


    new_path <- make_excel_file(
      list(
        Data = mtcars
      )
    )


    sha256_result <- compare_excel_versions(
      old_path,
      new_path,
      algo = "sha256",
      verbose = FALSE
    )


    md5_result <- compare_excel_versions(
      old_path,
      new_path,
      algo = "md5",
      verbose = FALSE
    )


    expect_equal(
      nchar(sha256_result$hash_old),
      64
    )


    expect_equal(
      nchar(md5_result$hash_old),
      32
    )

  })



# =============================================================================
# VALIDATION
# =============================================================================

test_that(
  "fails safely on missing files",
  {

    expect_error(
      compare_excel_versions(
        "missing_old.xlsx",
        "missing_new.xlsx",
        verbose = FALSE
      ),
      "does not exist|not found"
    )

  })



test_that(
  "rejects invalid algorithm",
  {

    old_path <- make_excel_file(
      list(
        Data = mtcars
      )
    )


    new_path <- make_excel_file(
      list(
        Data = mtcars
      )
    )


    expect_error(
      compare_excel_versions(
        old_path,
        new_path,
        algo = "invalid_algo",
        verbose = FALSE
      )
    )

  })



# =============================================================================
# CORRUPTED FILES
# =============================================================================

test_that(
  "fails safely on corrupted excel files",
  {

    old_path <- tempfile(
      fileext = ".xlsx"
    )


    new_path <- tempfile(
      fileext = ".xlsx"
    )


    writeLines(
      "not a real excel file",
      old_path
    )


    writeLines(
      "not a real excel file",
      new_path
    )


    expect_error(
      compare_excel_versions(
        old_path,
        new_path,
        verbose = FALSE
      ),
      "Failed to generate snapshot"
    )

  })



# =============================================================================
# DETERMINISM
# =============================================================================

test_that(
  "comparison is deterministic across runs",
  {

    old_path <- make_excel_file(
      list(
        Data = data.frame(
          x = 1:10
        )
      )
    )


    new_path <- make_excel_file(
      list(
        Data = data.frame(
          x = 1:10
        )
      )
    )


    r1 <- compare_excel_versions(
      old_path,
      new_path,
      verbose = FALSE
    )


    r2 <- compare_excel_versions(
      old_path,
      new_path,
      verbose = FALSE
    )


    expect_equal(
      r1$status,
      r2$status
    )


    expect_equal(
      r1$changed,
      r2$changed
    )


    expect_equal(
      r1$hash_old,
      r2$hash_old
    )


    expect_equal(
      r1$hash_new,
      r2$hash_new
    )

  })



# =============================================================================
# MANY SHEETS
# =============================================================================

test_that(
  "handles many sheets robustly",
  {

    sheets_old <- setNames(
      replicate(
        20,
        data.frame(
          x = 1:10
        ),
        simplify = FALSE
      ),
      paste0(
        "Sheet_",
        seq_len(20)
      )
    )


    sheets_new <- sheets_old


    sheets_new[[10]] <- data.frame(
      x = c(
        1:9,
        999
      )
    )


    old_path <- make_excel_file(
      sheets_old
    )


    new_path <- make_excel_file(
      sheets_new
    )


    result <- compare_excel_versions(
      old_path,
      new_path,
      verbose = FALSE
    )


    expect_equal(
      nrow(result),
      20
    )


    expect_equal(
      sum(result$changed),
      1
    )

  })



# =============================================================================
# UNICODE SHEETS
# =============================================================================

test_that(
  "handles unicode and unusual sheet names safely",
  {

    old_path <- make_excel_file(
      list(
        "São Paulo" = data.frame(x = 1),
        "测试" = data.frame(y = 2),
        "δοκιμή" = data.frame(z = 3)
      )
    )


    new_path <- make_excel_file(
      list(
        "São Paulo" = data.frame(x = 1),
        "测试" = data.frame(y = 999),
        "δοκιμή" = data.frame(z = 3)
      )
    )


    expect_no_error({

      result <- compare_excel_versions(
        old_path,
        new_path,
        verbose = FALSE
      )

    })


    expect_equal(
      sum(result$changed),
      1
    )

  })
