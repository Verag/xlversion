#' Normalize Text for Data Processing
#'
#' Applies a sequence of text normalization steps commonly used in data cleaning,
#' including accent removal, case standardization, whitespace trimming, and punctuation removal.
#'
#' @param x Character vector.
#' @param lowercase Logical. Convert text to lowercase.
#' @param remove_accents Logical. Remove accents using Unicode transliteration.
#' @param remove_punct Logical. Remove punctuation.
#' @param remove_numbers Logical. Remove digits.
#' @param trim Logical. Trim leading/trailing whitespace.
#' @param squash Logical. Replace multiple spaces with a single space.
#'
#' @return A cleaned character vector.
#' @export
normalize_text <- function(
    x,
    lowercase = TRUE,
    remove_accents = TRUE,
    remove_punct = TRUE,
    remove_numbers = FALSE,
    trim = TRUE,
    squash = TRUE
) {

  if (!is.character(x)) {
    x <- as.character(x)
  }

  # -------------------------
  # ACCENT REMOVAL (robust)
  # -------------------------

  if (remove_accents) {
    x <- stringi::stri_trans_general(x, "Latin-ASCII")
  }

  # -------------------------
  # LOWERCASE
  # -------------------------

  if (lowercase) {
    x <- tolower(x)
  }

  # -------------------------
  # REMOVE PUNCTUATION
  # -------------------------

  if (remove_punct) {
    x <- gsub("[[:punct:]]", " ", x)
  }

  # -------------------------
  # REMOVE NUMBERS
  # -------------------------

  if (remove_numbers) {
    x <- gsub("[0-9]", " ", x)
  }

  # -------------------------
  # TRIM WHITESPACE
  # -------------------------

  if (trim) {
    x <- trimws(x)
  }

  # -------------------------
  # NORMALIZE SPACES
  # -------------------------

  if (squash) {
    x <- gsub("\\s+", " ", x)
  }

  return(x)
}
