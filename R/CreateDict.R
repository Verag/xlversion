#' Create a Data Dictionary with Profiling Information
#'
#' Generates a structured data dictionary from a data.frame, including
#' variable types, missingness, summary statistics, and simple role classification.
#'
#' The function is designed for data profiling, quality assessment,
#' and exploratory analysis workflows.
#'
#' @param raw_data A non-empty data.frame to be analysed.
#' @param output_file Optional character string. Path to save the dictionary as an Excel file.
#' If NULL (default), no file is written.
#' @param verbose Logical. If TRUE, prints a message when exporting the dictionary.
#'
#' @return A data.frame of class \code{data_dict} containing:
#' \itemize{
#'   \item column: variable name
#'   \item type: inferred variable type (numeric, factor, character, datetime, etc.)
#'   \item n_unique: number of unique values
#'   \item na_rate: proportion of missing values
#'   \item role: heuristic classification (id_candidate, high_missing, normal)
#'   \item summary: compact textual summary of key statistics
#' }
#'
#' @details
#' Numeric variables include mean, standard deviation, min, and max.
#' Character and factor variables include most frequent values.
#' Datetime variables include range (min/max).
#'
#' Variables are automatically classified into:
#' \itemize{
#'   \item \code{id_candidate}: all values are unique
#'   \item \code{high_missing}: more than 20\% missing values
#'   \item \code{normal}: all other cases
#' }
#'
#' @importFrom openxlsx write.xlsx
#'
#' @export
#'
#' @examples
#' df <- data.frame(
#'   id = 1:5,
#'   value = c(10, 20, NA, 40, 50),
#'   group = c("A", "B", "A", "C", "B")
#' )
#'
create_dict <- function(raw_data, output_file = NULL, verbose = TRUE) {

  if (!is.data.frame(raw_data) || nrow(raw_data) == 0) {
    stop("Input must be a non-empty data.frame")
  }

  # ---- helpers ----

  detect_type <- function(x) {

    if (inherits(x, c("POSIXct", "POSIXt"))) return("datetime")
    if (is.character(x)) return("character")
    if (is.factor(x)) return("factor")
    if (is.numeric(x)) return("numeric")
    if (is.logical(x)) return("logical")

    "other"
  }

  summarise_var <- function(x, type) {

    na_rate <- mean(is.na(x))
    n_unique <- length(unique(x))

    base <- list(
      n_unique = n_unique,
      na_rate = round(na_rate, 3)
    )

    if (type == "numeric") {
      base$mean <- mean(x, na.rm = TRUE)
      base$sd <- stats::sd(x, na.rm = TRUE)
      base$min <- min(x, na.rm = TRUE)
      base$max <- max(x, na.rm = TRUE)
    }

    if (type %in% c("character", "factor")) {
      base$top_values <- paste(
        utils::head(names(sort(table(x), decreasing = TRUE)), 3),
        collapse = ", "
      )
    }

    if (type == "datetime") {
      base$min <- min(x, na.rm = TRUE)
      base$max <- max(x, na.rm = TRUE)
    }

    if (n_unique == nrow(raw_data)) {
      base$role <- "id_candidate"
    } else if (na_rate > 0.2) {
      base$role <- "high_missing"
    } else {
      base$role <- "normal"
    }

    base
  }

  # ---- main computation ----

  types <- vapply(raw_data, detect_type, character(1))

  dict <- lapply(names(raw_data), function(col) {

    x <- raw_data[[col]]
    type <- types[[col]]
    s <- summarise_var(x, type)

    data.frame(
      column = col,
      type = type,
      n_unique = s$n_unique,
      na_rate = s$na_rate,
      role = s$role,
      summary = paste(names(s), s, collapse = "; "),
      stringsAsFactors = FALSE
    )
  })

  dict <- do.call(rbind, dict)

  # ---- optional export ----
  if (!is.null(output_file)) {
    openxlsx::write.xlsx(dict, output_file, overwrite = TRUE)
    if (verbose) message("Dictionary exported to: ", output_file)
  }

  class(dict) <- c("data_dict", class(dict))

  return(dict)
}
