
# =============================================================================
# CONTRACT: TYPE + LENGTH + NA SAFETY
# =============================================================================

test_that("normalize_text preserves length and type", {

  input <- c("Olá", NA, "Mundo", "")

  result <- normalize_text(input)

  expect_type(result, "character")
  expect_length(result, length(input))
})

test_that("NA values are preserved", {

  input <- c("A", NA, "B")

  result <- normalize_text(input)

  expect_true(is.na(result[2]))
})

# =============================================================================
# DEFAULT BEHAVIOUR INVARIANTS
# =============================================================================

test_that("default settings produce cleaned lowercase text", {

  input <- c("Olá, Mundo! 123")

  result <- normalize_text(input)

  expect_false(grepl("[[:punct:]]", result))
  expect_true(grepl("[0-9]", result))
  expect_true(all(result == tolower(result)))
})

# =============================================================================
# ACCENT REMOVAL INVARIANT (NOT STRING-EXACT)
# =============================================================================

test_that("accent removal removes non-ascii characters", {

  input <- c("café naïve São João")

  result <- normalize_text(input)

  expect_false(any(grepl("[áàâãéêíóôõúç]", result, ignore.case = TRUE)))
})

# =============================================================================
# PUNCTUATION HANDLING
# =============================================================================

test_that("punctuation is fully removed when enabled", {

  input <- "Olá, mundo!!! (teste)."

  result <- normalize_text(input)

  expect_false(grepl("[[:punct:]]", result))
})

test_that("punctuation is preserved when disabled", {

  input <- "Olá, mundo!"

  result <- normalize_text(input, remove_punct = FALSE)

  expect_true(grepl(",", result) || grepl("!", result))
})

# =============================================================================
# NUMBERS HANDLING
# =============================================================================

test_that("numbers removed only when requested", {

  input <- "abc123def"

  r1 <- normalize_text(input, remove_numbers = TRUE)
  r2 <- normalize_text(input, remove_numbers = FALSE)

  expect_false(grepl("[0-9]", r1))
  expect_true(grepl("[0-9]", r2))
})

# =============================================================================
# WHITESPACE NORMALIZATION
# =============================================================================

test_that("squash collapses multiple spaces", {

  input <- "a    b     c"

  result <- normalize_text(input, squash = TRUE)

  expect_false(grepl("\\s{2,}", result))
})

test_that("trim removes leading/trailing spaces", {

  input <- "   abc   "

  result <- normalize_text(input, trim = TRUE, squash = FALSE)

  expect_false(grepl("^\\s|\\s$", result))
})

# =============================================================================
# PARAMETER COMBINATIONS (BEHAVIOUR MATRIX)
# =============================================================================

test_that("all parameters disabled returns character coercion only", {

  input <- c("Olá, Mundo!")

  result <- normalize_text(
    input,
    lowercase = FALSE,
    remove_accents = FALSE,
    remove_punct = FALSE,
    remove_numbers = FALSE,
    trim = FALSE,
    squash = FALSE
  )

  expect_equal(result, as.character(input))
})

# =============================================================================
# COERCION BEHAVIOUR
# =============================================================================

test_that("non-character input is safely coerced", {

  input <- list("Olá", 123, TRUE, NA)

  result <- normalize_text(input)

  expect_type(result, "character")
  expect_length(result, 4)
})

# =============================================================================
# DETERMINISM
# =============================================================================

test_that("function is deterministic", {

  input <- c("Café naïve João 123")

  r1 <- normalize_text(input)
  r2 <- normalize_text(input)

  expect_identical(r1, r2)
})

# =============================================================================
# PERFORMANCE SAFETY (CI / INSURANCE SCALE)
# =============================================================================

test_that("handles large vectors efficiently", {

  input <- rep("Café naïve João 123!", 5000)

  expect_no_error({
    result <- normalize_text(input)
  })

  expect_length(result, 5000)
})

# =============================================================================
# STRINGI DEPENDENCY SAFETY
# =============================================================================

test_that("stringi is available", {

  skip_if_not_installed("stringi")

  input <- "café naïve"

  result <- normalize_text(input)

  expect_true(is.character(result))
})
