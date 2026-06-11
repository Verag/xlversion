
library(testthat)


### Global tests configuration

test_dir <- here::here("tests", "testthat")

testthat::local_edition(3)   # to use 3 rd edition

# Options to more clean tests
options(
  warn = 1,                                   # show warnings during tests
  dplyr.summarise.inform = FALSE,            # mute dplyr message
  readxl.show_progress = FALSE
)


### Run all tests

cat("Initializing tests...\n\n")

test_results <- testthat::test_dir(
  path = test_dir,
  reporter = testthat::MultiReporter$new(
    reporters = list(
      testthat::ProgressReporter$new(),
      testthat::SummaryReporter$new()
    )
  ),
  stop_on_failure = TRUE,
  stop_on_warning = FALSE
)

cat("\n Tests completed \n\n")
print(test_results)
