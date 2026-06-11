
testthat::skip_if_not_installed("withr")
testthat::skip_if_not_installed("digest")
testthat::skip_if_not_installed("rlang")

# -------------------------------------------------------------------------
# HELPER
# -------------------------------------------------------------------------

make_temp_file <- function(content = "test", age_days = 0) {

  path <- tempfile(fileext = ".txt")
  writeLines(content, path)

  Sys.setFileTime(
    path,
    Sys.time() - age_days * 86400
  )

  path
}

# -------------------------------------------------------------------------
# INPUT VALIDATION
# -------------------------------------------------------------------------

testthat::test_that("invalid inputs are rejected", {

  testthat::expect_error(
    is_file_stale(NA),
    class = "is_file_stale_invalid_path"
  )

  testthat::expect_error(
    is_file_stale("x", max_age_days = -1),
    class = "is_file_stale_invalid_max_age"
  )

  testthat::expect_error(
    is_file_stale("x", use_hash = "yes"),
    class = "is_file_stale_invalid_use_hash"
  )

  testthat::expect_error(
    is_file_stale("x", verbose = 1),
    class = "is_file_stale_invalid_verbose"
  )

})

# -------------------------------------------------------------------------
# FILE NOT FOUND
# -------------------------------------------------------------------------

testthat::test_that("missing file triggers correct error", {

  testthat::expect_error(
    is_file_stale("non_existing_file.txt"),
    class = "is_file_stale_file_not_found"
  )

})

# -------------------------------------------------------------------------
# STRUCTURE CONTRACT
# -------------------------------------------------------------------------

testthat::test_that("returns correct structure", {

  path <- make_temp_file()

  result <- is_file_stale(path, verbose = FALSE)

  testthat::expect_type(result, "list")

  testthat::expect_named(
    result,
    c("path", "real_path", "age_days", "hash", "outdated"),
    ignore.order = FALSE
  )

  testthat::expect_equal(result$path, path)

  testthat::expect_type(result$real_path, "character")

})

# -------------------------------------------------------------------------
# STALENESS LOGIC
# -------------------------------------------------------------------------

testthat::test_that("detects stale files correctly", {

  path <- make_temp_file(age_days = 10)

  result <- is_file_stale(
    path,
    max_age_days = 7,
    verbose = FALSE
  )

  testthat::expect_true(result$outdated)
  testthat::expect_gt(result$age_days, 7)
  testthat::expect_equal(nchar(result$hash), 64)

})

testthat::test_that("detects fresh files correctly", {

  path <- make_temp_file(age_days = 1)

  result <- is_file_stale(
    path,
    max_age_days = 7,
    verbose = FALSE
  )

  testthat::expect_false(result$outdated)

})

# -------------------------------------------------------------------------
# HASH BEHAVIOUR
# -------------------------------------------------------------------------

testthat::test_that("hash is deterministic", {

  path <- make_temp_file("stable-content")

  hashes <- replicate(
    5,
    is_file_stale(path, verbose = FALSE)$hash
  )

  testthat::expect_length(unique(hashes), 1)

})

testthat::test_that("hash changes with content", {

  path <- make_temp_file("v1")

  h1 <- is_file_stale(path, verbose = FALSE)$hash

  writeLines("v2", path)

  h2 <- is_file_stale(path, verbose = FALSE)$hash

  testthat::expect_false(identical(h1, h2))

})

testthat::test_that("hash is removed when disabled", {

  path <- make_temp_file()

  result <- is_file_stale(
    path,
    use_hash = FALSE,
    verbose = FALSE
  )

  testthat::expect_false("hash" %in% names(result))

})

# -------------------------------------------------------------------------
# WARNINGS (rlang-based)
# -------------------------------------------------------------------------

testthat::test_that("emits structured warning when stale", {

  path <- make_temp_file(age_days = 30)

  testthat::expect_warning(
    is_file_stale(path, max_age_days = 7, verbose = TRUE),
    class = "is_file_stale_stale_file"
  )

})

testthat::test_that("no warning for fresh files", {

  path <- make_temp_file(age_days = 1)

  testthat::expect_no_warning(
    is_file_stale(path, max_age_days = 7, verbose = TRUE)
  )

})

# -------------------------------------------------------------------------
# INVISIBILITY
# -------------------------------------------------------------------------

testthat::test_that("returns invisibly", {

  path <- make_temp_file()

  res <- withVisible(
    is_file_stale(path, verbose = FALSE)
  )

  testthat::expect_false(res$visible)

})

# -------------------------------------------------------------------------
# EDGE CASES (ENTERPRISE / INSURANCE STYLE)
# -------------------------------------------------------------------------

testthat::test_that("handles empty file", {

  path <- tempfile(fileext = ".txt")
  file.create(path)

  result <- is_file_stale(path, verbose = FALSE)

  testthat::expect_type(result$hash, "character")

})

testthat::test_that("handles large file", {

  path <- tempfile(fileext = ".txt")

  writeLines(
    paste(rep("claim_event", 1e5), collapse = "\n"),
    path
  )

  result <- is_file_stale(path, verbose = FALSE)

  testthat::expect_type(result$hash, "character")

})

testthat::test_that("handles UTF-8 filenames", {

  path <- file.path(tempdir(), "sinistro_ação_東京.txt")

  writeLines("data", path)

  result <- is_file_stale(path, verbose = FALSE)

  testthat::expect_type(result$real_path, "character")

})

# -------------------------------------------------------------------------
# FUTURE TIMESTAMP
# -------------------------------------------------------------------------

testthat::test_that("handles future timestamps", {

  path <- make_temp_file()

  Sys.setFileTime(path, Sys.time() + 86400)

  result <- is_file_stale(path, verbose = FALSE)

  testthat::expect_lt(result$age_days, 0)

})

# -------------------------------------------------------------------------
# EXTREME THRESHOLD
# -------------------------------------------------------------------------

testthat::test_that("handles extreme max_age_days", {

  path <- make_temp_file(age_days = 100)

  result <- is_file_stale(
    path,
    max_age_days = 1e12,
    verbose = FALSE
  )

  testthat::expect_false(result$outdated)

})

# -------------------------------------------------------------------------
# PERMISSION ERROR (UPDATED - CRITICAL FIX)
# -------------------------------------------------------------------------

testthat::test_that("permission denied triggers correct error", {

  testthat::skip_on_os("windows")

  path <- make_temp_file("secure")

  Sys.chmod(path, "0000")

  withr::defer(Sys.chmod(path, "0600"))

  testthat::expect_error(
    is_file_stale(path, verbose = FALSE),
    class = "is_file_stale_permission_denied"
  )

})

# -------------------------------------------------------------------------
# SYMLINK HANDLING
# -------------------------------------------------------------------------

testthat::test_that("handles symlinks safely", {

  testthat::skip_on_os("windows")

  real <- make_temp_file("data")

  link <- file.path(tempdir(), "link_file")

  file.symlink(real, link)

  withr::defer(unlink(link))

  result <- is_file_stale(link, verbose = FALSE)

  testthat::expect_type(result, "list")

})

# -------------------------------------------------------------------------
# DETERMINISM
# -------------------------------------------------------------------------

testthat::test_that("function is stable across repeated calls", {

  path <- make_temp_file("stable-content")

  r1 <- is_file_stale(path, verbose = FALSE)
  r2 <- is_file_stale(path, verbose = FALSE)

  testthat::expect_identical(r1$hash, r2$hash)
  testthat::expect_identical(r1$outdated, r2$outdated)
  testthat::expect_equal(r1$age_days, r2$age_days, tolerance = 1e-6)

})
