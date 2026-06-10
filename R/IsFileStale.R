#' Check if a file is stale
#'
#' Robust CRAN-safe file staleness checker with optional hashing.
#'
#' @param path Character. File path.
#' @param max_age_days Numeric scalar. Maximum allowed age in days.
#' @param use_hash Logical. Whether to compute SHA256 hash.
#' @param verbose Logical. Whether to emit warnings.
#'
#' @return A list (invisibly) with file metadata.
#'
#' @export
#' Check if a file is stale (CRAN-safe version)
is_file_stale <- function(path,
                          max_age_days = 1,
                          use_hash = TRUE,
                          verbose = TRUE) {

  # ---------------------------------------------------------
  # 1. INPUT VALIDATION (fast fail, no IO yet)
  # ---------------------------------------------------------

  if (!is.character(path) || length(path) != 1 || is.na(path)) {
    rlang::abort(
      "Argument `path` must be a single non-NA character string.",
      class = "is_file_stale_invalid_path"
    )
  }

  if (!is.numeric(max_age_days) || length(max_age_days) != 1 || max_age_days < 0) {
    rlang::abort(
      "`max_age_days` must be a single non-negative numeric value.",
      class = "is_file_stale_invalid_max_age"
    )
  }

  if (!is.logical(use_hash) || length(use_hash) != 1) {
    rlang::abort(
      "`use_hash` must be TRUE or FALSE.",
      class = "is_file_stale_invalid_use_hash"
    )
  }

  if (!is.logical(verbose) || length(verbose) != 1) {
    rlang::abort(
      "`verbose` must be TRUE or FALSE.",
      class = "is_file_stale_invalid_verbose"
    )
  }

  # ---------------------------------------------------------
  # 2. FILE EXISTENCE + ACCESSIBILITY (CRITICAL FIX)
  # ---------------------------------------------------------

  if (!file.exists(path)) {
    rlang::abort(
      paste0("File not found: ", path),
      class = "is_file_stale_file_not_found"
    )
  }

  # Check read permission BEFORE hashing
  if (file.access(path, 4) != 0) {
    rlang::abort(
      paste0("File is not readable: ", path),
      class = "is_file_stale_permission_denied"
    )
  }

  # Resolve symlinks safely
  real_path <- tryCatch(
    normalizePath(path, winslash = "/", mustWork = TRUE),
    error = function(e) path
  )

  # ---------------------------------------------------------
  # 3. METADATA (safe now)
  # ---------------------------------------------------------

  file_time <- tryCatch(
    file.mtime(real_path),
    error = function(e) {
      rlang::abort(
        paste0("Cannot read file metadata: ", path),
        class = "is_file_stale_metadata_error"
      )
    }
  )

  age_days <- as.numeric(
    difftime(Sys.time(), file_time, units = "days")
  )

  outdated <- age_days > max_age_days

  # ---------------------------------------------------------
  # 4. HASH (only after safe checks)
  # ---------------------------------------------------------

  if (use_hash) {

    hash <- tryCatch(
      digest::digest(file = real_path, algo = "sha256"),
      error = function(e) {
        rlang::abort(
          paste0("Failed to compute file hash: ", path),
          class = "is_file_stale_hash_error"
        )
      }
    )

    result <- list(
      path = path,
      real_path = real_path,
      age_days = age_days,
      hash = hash,
      outdated = outdated
    )

  } else {

    result <- list(
      path = path,
      real_path = real_path,
      age_days = age_days,
      outdated = outdated
    )

  }

  # ---------------------------------------------------------
  # 5. WARNINGS (controlled, non-fatal)
  # ---------------------------------------------------------

  if (verbose && isTRUE(outdated)) {

    rlang::warn(
      sprintf(
        "File is STALE: %s (%.2f days old)",
        path,
        age_days
      ),
      class = "is_file_stale_stale_file"
    )

  }

  # ---------------------------------------------------------
  # 6. OUTPUT
  # ---------------------------------------------------------

  invisible(result)
}
