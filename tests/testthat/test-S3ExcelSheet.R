# =============================================================================
# TEST FILE: excel_sheet S3 Class
# =============================================================================

testthat::test_that("new_excel_sheet creates valid object with cached metadata", {
  df <- data.frame(x = 1:10, y = letters[1:10])
  sheet_obj <- new_excel_sheet(df, "MySheet")

  testthat::expect_s3_class(sheet_obj, "excel_sheet")
  testthat::expect_equal(sheet_obj$sheet, "MySheet")
  testthat::expect_equal(sheet_obj$n_rows, 10)
  testthat::expect_equal(sheet_obj$n_cols, 2)
})

testthat::test_that("print.excel_sheet produces informative output", {
  sheet_obj <- new_excel_sheet(data.frame(a = 1:5), "TestSheet")

  expect_output(print(sheet_obj), "<excel_sheet>")
  expect_output(print(sheet_obj), "TestSheet")
  expect_output(print(sheet_obj), "5 rows × 1 columns")
})

testthat::test_that("as.data.frame.excel_sheet returns the underlying data", {
  df <- data.frame(x = 1:5, y = 6:10)
  sheet_obj <- new_excel_sheet(df, "Data")

  df_out <- as.data.frame(sheet_obj)
  testthat::expect_equal(df_out, df)
  testthat::expect_s3_class(df_out, "data.frame")
})

testthat::test_that("get_data extracts data correctly", {
  df <- data.frame(val = rnorm(10))
  sheet_obj <- new_excel_sheet(df, "Test")

  extracted <- get_data(sheet_obj)
  testthat::expect_equal(extracted, df)
})
