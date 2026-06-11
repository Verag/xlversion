
library(testthat)

test_that("Full versioning workflow works end-to-end", {

  # Created temporary files
  tmp_v1 <- tempfile(fileext = ".xlsx")
  tmp_v2 <- tempfile(fileext = ".xlsx")

  # Version 1
  data_v1 <- list(
    Clientes = data.frame(ID = 1:10, Nome = paste("Cliente", 1:10)),
    Vendas   = data.frame(Data = Sys.Date() - 1:5, Valor = 100:104)
  )
  writexl::write_xlsx(data_v1, tmp_v1)

  # Version 2 (with changes)
  data_v2 <- list(
    Clientes = data.frame(ID = 1:12, Nome = paste("Cliente", 1:12)), # +2 rows
    Vendas   = data.frame(Data = Sys.Date() - 1:5, Valor = c(100:103, 999)) # 1 value changed
  )
  writexl::write_xlsx(data_v2, tmp_v2)

  # Test all flow
  expect_no_error({
    snap1 <- snapshot_excel(tmp_v1)
    snap2 <- snapshot_excel(tmp_v2)

    comparison <- compare_excel_versions(
      old_path = tmp_v1,
      new_path = tmp_v2,
      ignore_column_order = TRUE
    )
  })

  # Validations
  expect_gt(nrow(comparison), 0)
  expect_true(any(comparison$status %in% c("Changed", "New sheet")))

  # Test with  only_changes = TRUE
  changes_only <- compare_excel_versions(tmp_v1, tmp_v2, only_changes = TRUE)
  expect_true(all(changes_only$status != "Unchanged"))
})

test_that("Versioning workflow with specific sheets works", {
  tmp_old <- tempfile(fileext = ".xlsx")
  tmp_new <- tempfile(fileext = ".xlsx")

  writexl::write_xlsx(list(
    Dashboard = mtcars[1:10, ],
    RawData   = iris
  ), tmp_old)

  writexl::write_xlsx(list(
    Dashboard = mtcars[1:15, ],   # more rows
    RawData   = iris
  ), tmp_new)

  comp <- compare_excel_versions(
    old_path = tmp_old,
    new_path = tmp_new,
    old_sheets = "Dashboard",
    new_sheets = "Dashboard"
  )

  expect_equal(nrow(comp), 1)
  expect_equal(comp$status, "Changed")
})

# =============================================================================
# Performance & Scalability
# =============================================================================

test_that("Functions handle reasonably sized Excel files", {
  tmp <- tempfile(fileext = ".xlsx")

  large_df <- data.frame(
    id = 1:5000,
    text = paste("Texto de teste com acentuação", 1:5000),
    value = rnorm(5000)
  )

  writexl::write_xlsx(large_df, tmp)

  expect_no_error({
    fp <- file_fingerprint(tmp, sheet = 1)
    snap <- snapshot_excel(tmp)
  })

  expect_true(nchar(fp$hash) > 0)
})
