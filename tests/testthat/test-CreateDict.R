testthat::skip_if_not_installed("fabricatr")
testthat::skip_if_not_installed("withr")

# -------------------------------------------------------------------------
# HELPERS
# -------------------------------------------------------------------------

make_numeric_df <- function(n = 100) {

  fabricatr::fabricate(
    N = n,
    policy_id = sprintf("POL%010d", seq_len(N)),
    value = rnorm(N, mean = 100, sd = 10),
    group = sample(c("A", "B", "C"), N, replace = TRUE)
  )


}

# -------------------------------------------------------------------------
# INPUT VALIDATION
# -------------------------------------------------------------------------

testthat::test_that("create_dict rejects invalid input", {

  testthat::expect_error(
    create_dict(NULL),
    "Input must be a non-empty data.frame"
  )

  testthat::expect_error(
    create_dict(data.frame()),
    "Input must be a non-empty data.frame"
  )

})

# -------------------------------------------------------------------------
# BASIC STRUCTURE
# -------------------------------------------------------------------------

testthat::test_that("create_dict returns expected structure", {

  withr::with_seed(101, {

    df <- make_numeric_df()

    result <- create_dict(df, verbose = FALSE)

  })

  expected_cols <- c(
    "column",
    "type",
    "n_unique",
    "na_rate",
    "role",
    "summary"
  )

  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(inherits(result, "data_dict"))

  testthat::expect_named(
    result,
    expected_cols,
    ignore.order = TRUE
  )

  testthat::expect_equal(
    nrow(result),
    ncol(df)
  )

})

# -------------------------------------------------------------------------
# TYPE DETECTION
# -------------------------------------------------------------------------

testthat::test_that("create_dict detects variable types correctly", {

  withr::with_seed(202, {

    df <- fabricatr::fabricate(
      N = 50,
      num = rnorm(N),
      char = sample(letters[1:3], N, replace = TRUE),
      logi = sample(c(TRUE, FALSE), N, replace = TRUE)
    )

  })

  result <- create_dict(df, verbose = FALSE)

  testthat::expect_equal(
    result$type[result$column == "num"],
    "numeric"
  )

  testthat::expect_equal(
    result$type[result$column == "char"],
    "character"
  )

  testthat::expect_equal(
    result$type[result$column == "logi"],
    "logical"
  )

})

# -------------------------------------------------------------------------
# MISSING VALUES
# -------------------------------------------------------------------------

testthat::test_that("create_dict computes na_rate correctly", {

  withr::with_seed(303, {

    df <- fabricatr::fabricate(
      N = 100,
      x = ifelse(runif(N) < 0.5, NA, rnorm(N))
    )

  })

  result <- create_dict(df, verbose = FALSE)

  observed_rate <- result$na_rate[result$column == "x"]

  testthat::expect_gt(observed_rate, 0.4)
  testthat::expect_lt(observed_rate, 0.6)

})

# -------------------------------------------------------------------------
# ROLE DETECTION
# -------------------------------------------------------------------------

testthat::test_that("create_dict detects id_candidate variables", {

  withr::with_seed(404, {

    df <- fabricatr::fabricate(
      N = 50,
      id = sprintf("POL%010d", seq_len(N))
    )

  })

  result <- create_dict(df, verbose = FALSE)

  testthat::expect_equal(
    result$role[result$column == "id"],
    "id_candidate"
  )

})

testthat::test_that("create_dict detects high_missing variables", {

  withr::with_seed(505, {

    df <- fabricatr::fabricate(
      N = 100,
      x = ifelse(runif(N) < 0.8, NA, rnorm(N))
    )

  })

  result <- create_dict(df, verbose = FALSE)

  testthat::expect_equal(
    result$role[result$column == "x"],
    "high_missing"
  )

})

# -------------------------------------------------------------------------
# TOP VALUES
# -------------------------------------------------------------------------

testthat::test_that("create_dict extracts top values correctly", {

  withr::with_seed(606, {

    df <- fabricatr::fabricate(
      N = 100,
      x = sample(
        c("A", "B", "C"),
        N,
        replace = TRUE,
        prob = c(0.7, 0.2, 0.1)
      )
    )

  })

  result <- create_dict(df, verbose = FALSE)

  summary_text <- result$summary[result$column == "x"]

  testthat::expect_match(summary_text, "A")

})

# -------------------------------------------------------------------------
# DATETIME HANDLING
# -------------------------------------------------------------------------

testthat::test_that("create_dict handles datetime variables", {

  withr::with_seed(707, {

    df <- fabricatr::fabricate(
      N = 50,
      dt = as.POSIXct(Sys.time() + seq_len(N))
    )

  })

  result <- create_dict(df, verbose = FALSE)

  testthat::expect_equal(
    result$type[result$column == "dt"],
    "datetime"
  )

})

# -------------------------------------------------------------------------
# EXPORT
# -------------------------------------------------------------------------

testthat::test_that("create_dict exports file when requested", {

  withr::local_tempfile(
    lines = NULL,
    fileext = ".xlsx"
  ) -> test_file

  withr::with_seed(808, {

    df <- fabricatr::fabricate(
      N = 20,
      x = rnorm(N)
    )

  })

  create_dict(
    df,
    output_file = test_file,
    verbose = FALSE
  )

  testthat::expect_true(file.exists(test_file))

})

# -------------------------------------------------------------------------
# DETERMINISM
# -------------------------------------------------------------------------

testthat::test_that("create_dict is deterministic", {

  withr::with_seed(909, {

    df <- fabricatr::fabricate(
      N = 50,
      x = rnorm(N)
    )

  })

  r1 <- create_dict(df, verbose = FALSE)
  r2 <- create_dict(df, verbose = FALSE)

  testthat::expect_identical(r1, r2)

})


# -------------------------------------------------------------------------
# HIGH CARDINALITY
# -------------------------------------------------------------------------

testthat::test_that("create_dict handles extremely high cardinality IDs", {

  withr::with_seed(1001, {

    df <- data.frame(
      policy_id = sprintf("POL%010d", seq_len(5000))
    )

  })

  result <- create_dict(df, verbose = FALSE)

  testthat::expect_equal(
    result$n_unique[result$column == "policy_id"],
    5000
  )

  testthat::expect_equal(
    result$role[result$column == "policy_id"],
    "id_candidate"
  )

})


# -------------------------------------------------------------------------
# MISSING VARIABLES
# -------------------------------------------------------------------------


testthat::test_that("create_dict handles almost entirely missing variables", {

  withr::with_seed(1002, {

    x <- rep(NA_real_, 1000)
    x[sample(1000, 3)] <- c(100, 200, 300)

    df <- data.frame(claim_amount = x)

  })

  result <- create_dict(df, verbose = FALSE)

  na_rate <- result$na_rate[result$column == "claim_amount"]

  testthat::expect_gt(na_rate, 0.99)

  testthat::expect_equal(
    result$role[result$column == "claim_amount"],
    "high_missing"
  )

})

# -------------------------------------------------------------------------
# EXTREMELY SKEWED DATA
# -------------------------------------------------------------------------

testthat::test_that("create_dict handles extremely skewed financial data", {

  withr::with_seed(1003, {
    df <- data.frame(
      claim_cost = c(rexp(9999, rate = 1 / 500), 5e+07)
    )
  })

  result <- create_dict(df, verbose = FALSE)
  col_info <- result[result$column == "claim_cost", ]

  # Core expectations
  testthat::expect_equal(col_info$type, "numeric")

  # Robust check for the large outlier
  summary_lower <- tolower(col_info$summary)

  testthat::expect_true(
    any(
      grepl("5e\\+?07", summary_lower),
      grepl("50000000", summary_lower),
      grepl("5.?e7", summary_lower),
      grepl("50,000,000", summary_lower)
    ),
    info = paste("Expected to find large outlier representation. Got:", col_info$summary)
  )

  # Optional: check that max value is correctly identified
  testthat::expect_true(
    grepl("max|maximum|outlier|5", summary_lower),
    info = "Summary should mention the extreme value"
  )
})

# -------------------------------------------------------------------------
# UTF-8 AND MULTILINGUAL VALUES
# -------------------------------------------------------------------------

testthat::test_that("create_dict handles UTF-8 and multilingual values", {

  df <- data.frame(
    customer_name = c(
      "José",
      "Müller",
      "François",
      "東京",
      "São Paulo",
      "Łódź"
    )
  )

  result <- create_dict(df, verbose = FALSE)

  testthat::expect_equal(
    result$type[result$column == "customer_name"],
    "character"
  )

})

# -------------------------------------------------------------------------
# DUPLICATED ROWS
# -------------------------------------------------------------------------

testthat::test_that("create_dict handles duplicated rows safely", {

  df <- data.frame(
    policy = rep("P123", 100),
    claim = rep(1000, 100)
  )

  result <- create_dict(df, verbose = FALSE)

  testthat::expect_equal(
    result$n_unique[result$column == "policy"],
    1
  )

})

# -------------------------------------------------------------------------
# EMPTY STRINGS AND WHITESPACE
# -------------------------------------------------------------------------

testthat::test_that("create_dict handles empty strings and whitespace", {

  df <- data.frame(
    broker = c("", " ", "   ", NA, "AON", "Marsh")
  )

  result <- create_dict(df, verbose = FALSE)

  testthat::expect_equal(
    result$type[result$column == "broker"],
    "character"
  )

})

# -------------------------------------------------------------------------
# COLUMN NAMES
# -------------------------------------------------------------------------

testthat::test_that("create_dict handles pathological column names", {

  df <- data.frame(
    "Claim Amount (€)" = rnorm(10),
    "POLICY-ID" = 1:10,
    check.names = FALSE
  )

  result <- create_dict(df, verbose = FALSE)

  testthat::expect_true(
    "Claim Amount (€)" %in% result$column
  )

  testthat::expect_true(
    "POLICY-ID" %in% result$column
  )

})

# -------------------------------------------------------------------------
# DATE AND TIMEZONE INCONSISTENCIES
# -------------------------------------------------------------------------

testthat::test_that("create_dict handles date and timezone inconsistencies", {

  df <- data.frame(
    event_time = as.POSIXct(
      c(
        "2024-01-01 10:00:00 UTC",
        "2024-01-01 10:00:00 CET",
        "2024-01-01 10:00:00 GMT"
      ),
      tz = "UTC"
    )
  )

  result <- create_dict(df, verbose = FALSE)

  testthat::expect_equal(
    result$type[result$column == "event_time"],
    "datetime"
  )

})

# -------------------------------------------------------------------------
# WIDE DATASETS
# -------------------------------------------------------------------------

testthat::test_that("create_dict handles very wide datasets", {

  withr::with_seed(1004, {

    df <- as.data.frame(
      replicate(
        500,
        rnorm(100)
      )
    )

  })

  result <- create_dict(df, verbose = FALSE)

  testthat::expect_equal(
    nrow(result),
    500
  )

})

# -------------------------------------------------------------------------
# ZERO VARIANCE VARIABLES
# -------------------------------------------------------------------------

testthat::test_that("create_dict handles zero variance variables", {

  df <- data.frame(
    deductible = rep(500, 1000)
  )

  result <- create_dict(df, verbose = FALSE)

  testthat::expect_equal(
    result$n_unique[result$column == "deductible"],
    1
  )

})

# -------------------------------------------------------------------------
# LIST COLUMNS
# -------------------------------------------------------------------------

testthat::test_that("create_dict handles list columns gracefully", {

  df <- data.frame(id = 1:3)

  df$nested <- list(
    list(a = 1),
    list(b = 2),
    list(c = 3)
  )

  testthat::expect_error(
    create_dict(df, verbose = FALSE),
    regexp = NA
  )

})

# -------------------------------------------------------------------------
# TIBBLE INPUTS
# -------------------------------------------------------------------------

testthat::test_that("create_dict handles tibble inputs", {

  testthat::skip_if_not_installed("tibble")

  df <- tibble::tibble(
    policy_id = 1:10,
    premium = runif(10)
  )

  result <- create_dict(df, verbose = FALSE)

  testthat::expect_s3_class(
    result,
    "data.frame"
  )

})

# -------------------------------------------------------------------------
# INFINITE VALUES
# -------------------------------------------------------------------------

testthat::test_that("create_dict handles infinite values safely", {

  df <- data.frame(
    exposure = c(1, 2, Inf, -Inf, NA)
  )

  result <- create_dict(df, verbose = FALSE)

  testthat::expect_equal(
    result$type[result$column == "exposure"],
    "numeric"
  )

})

# -------------------------------------------------------------------------
# LARGE SCALE INPUT
# -------------------------------------------------------------------------

testthat::test_that("create_dict remains deterministic under large scale input", {

  withr::with_seed(1005, {

    df <- data.frame(
      policy_id = seq_len(10000),
      premium = rgamma(10000, shape = 2),
      claim_count = rpois(10000, lambda = 0.2),
      region = sample(LETTERS, 10000, replace = TRUE)
    )

  })

  r1 <- create_dict(df, verbose = FALSE)
  r2 <- create_dict(df, verbose = FALSE)

  testthat::expect_identical(r1, r2)

})
