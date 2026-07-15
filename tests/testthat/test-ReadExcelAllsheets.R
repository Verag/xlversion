# =============================================================================
# TEST FILE: read_excel_allsheets
# =============================================================================

testthat::skip_if_not_installed("writexl")
testthat::skip_if_not_installed("readxl")
testthat::skip_if_not_installed("openxlsx")

# =============================================================================
# HELPERS
# =============================================================================

make_excel_file <- function(sheets) {

  path <- tempfile(fileext = ".xlsx")

  writexl::write_xlsx(
    sheets,
    path
  )

  path

}



make_corrupt_excel_file <- function() {

  path <- tempfile(fileext = ".xlsx")

  writeLines(
    "This is not a valid Excel workbook.",
    path
  )

  path

}



simple_workbook <- function() {

  make_excel_file(

    list(

      A = data.frame(
        x = 1:3
      ),

      B = data.frame(
        y = letters[1:3]
      ),

      C = data.frame(
        z = c(TRUE,FALSE,TRUE)
      )

    )

  )

}

# =============================================================================
# API CONTRACT
# =============================================================================

testthat::test_that(
  "read_excel_allsheets returns an excel_book object",
  {

    path <- simple_workbook()

    result <- read_excel_allsheets(
      path
    )

    testthat::expect_s3_class(
      result,
      "excel_book"
    )

  })



testthat::test_that(
  "worksheet order is preserved",
  {

    path <- simple_workbook()

    result <- read_excel_allsheets(
      path
    )

    testthat::expect_equal(

      names(result$sheets),

      c(
        "A",
        "B",
        "C"
      )

    )

  })



testthat::test_that(
  "workbook metadata are attached",
  {

    path <- simple_workbook()

    result <- read_excel_allsheets(
      path
    )

    testthat::expect_equal(

      result$metadata$filename,

      path

    )



    testthat::expect_equal(

      result$metadata$engine,

      "readxl"

    )



    testthat::expect_equal(

      result$metadata$available_sheets,

      c(
        "A",
        "B",
        "C"
      )

    )

  })

# =============================================================================
# INPUT VALIDATION
# =============================================================================

testthat::test_that(
  "rejects non-character filename",
  {

    testthat::expect_error(

      read_excel_allsheets(
        123
      ),

      "`filename` must be a single character string"

    )

  })



testthat::test_that(
  "fails when workbook does not exist",
  {

    testthat::expect_error(

      read_excel_allsheets(
        "does_not_exist.xlsx"
      ),

      "File does not exist"

    )

  })



testthat::test_that(
  "rejects unsupported file extensions",
  {

    txt <- tempfile(
      fileext = ".txt"
    )

    writeLines(
      "dummy",
      txt
    )

    testthat::expect_error(

      read_excel_allsheets(
        txt
      ),

      "Unsupported Excel file extension"

    )

  })



testthat::test_that(
  "rejects invalid engine",
  {

    path <- simple_workbook()

    testthat::expect_error(

      read_excel_allsheets(

        path,

        engine="abc"

      )

    )

  })



testthat::test_that(
  "rejects invalid guess_max values",
  {

    path <- simple_workbook()

    invalid <- list(

      0,

      -1,

      NA_real_,

      Inf,

      NaN,

      numeric(0)

    )



    for(x in invalid){

      testthat::expect_error(

        read_excel_allsheets(

          path,

          guess_max=x

        ),

        "guess_max"

      )

    }

  })



testthat::test_that(
  "rejects invalid sheet_names type",
  {

    path <- simple_workbook()

    testthat::expect_error(

      read_excel_allsheets(

        path,

        sheet_names=list("A")

      ),

      "sheet_names"

    )

  })



testthat::test_that(
  "rejects empty sheet_names",
  {

    path <- simple_workbook()

    testthat::expect_error(

      read_excel_allsheets(

        path,

        sheet_names=character(0)

      ),

      "cannot be empty"

    )



    testthat::expect_error(

      read_excel_allsheets(

        path,

        sheet_names=numeric(0)

      ),

      "cannot be empty"

    )

  })



testthat::test_that(
  "rejects invalid numeric sheet indices",
  {

    path <- simple_workbook()



    invalid <- list(

      0,

      -1,

      1.5,

      NA_real_,

      Inf,

      NaN

    )



    for(x in invalid){

      testthat::expect_error(

        read_excel_allsheets(

          path,

          sheet_names=x

        ),

        "sheet"

      )

    }

  })
testthat::test_that(
  "rejects logical arguments with invalid types",
  {

    path <- simple_workbook()

    testthat::expect_error(
      read_excel_allsheets(
        path,
        parallel = 1
      ),
      "`parallel` must be TRUE or FALSE\\."
    )

    testthat::expect_error(
      read_excel_allsheets(
        path,
        tibble = "TRUE"
      ),
      "`tibble` must be TRUE or FALSE\\."
    )

    testthat::expect_error(
      read_excel_allsheets(
        path,
        verbose = 1
      ),
      "`verbose` must be TRUE or FALSE\\."
    )

    testthat::expect_error(
      read_excel_allsheets(
        path,
        return_failed = 1
      ),
      "`return_failed` must be TRUE or FALSE\\."
    )

  }
)
# =============================================================================
# READING BEHAVIOUR
# =============================================================================

testthat::test_that(
  "reads all worksheets by default",
  {

    path <- simple_workbook()

    book <- read_excel_allsheets(
      path
    )

    testthat::expect_equal(

      names(book$sheets),

      c(
        "A",
        "B",
        "C"
      )

    )

  })



testthat::test_that(
  "reads selected worksheets by name",
  {

    path <- simple_workbook()

    book <- read_excel_allsheets(

      path,

      sheet_names = c(
        "A",
        "C"
      )

    )



    testthat::expect_equal(

      names(book$sheets),

      c(
        "A",
        "C"
      )

    )

  })



testthat::test_that(
  "reads selected worksheets by numeric index",
  {

    path <- simple_workbook()

    book <- read_excel_allsheets(

      path,

      sheet_names = c(
        1,
        3
      )

    )



    testthat::expect_equal(

      names(book$sheets),

      c(
        "A",
        "C"
      )

    )

  })



testthat::test_that(
  "fails when requested sheet does not exist",
  {

    path <- simple_workbook()

    testthat::expect_error(

      read_excel_allsheets(

        path,

        sheet_names = "UNKNOWN"

      ),

      "Requested sheets not found"

    )

  }
)



testthat::test_that(
  "rejects sheet indices outside workbook bounds",
  {

    path <- simple_workbook()

    testthat::expect_error(

      read_excel_allsheets(

        path,

        sheet_names = 10

      ),

      "Sheet index out of bounds"

    )

  })



testthat::test_that(
  "accepts tibble = FALSE",
  {

    path <- simple_workbook()

    testthat::expect_error(

      read_excel_allsheets(

        path,

        tibble = FALSE

      ),

      NA

    )

  })



testthat::test_that(
  "accepts tibble = TRUE",
  {

    path <- simple_workbook()

    testthat::expect_error(

      read_excel_allsheets(

        path,

        tibble = TRUE

      ),

      NA

    )

  })



testthat::test_that(
  "guess_max is accepted for valid values",
  {

    path <- simple_workbook()

    testthat::expect_error(

      read_excel_allsheets(

        path,

        guess_max = 5

      ),

      NA

    )

  })



testthat::test_that(
  "additional arguments are forwarded to the backend",
  {

    path <- make_excel_file(

      list(

        Sheet1 = data.frame(

          x = 1:10

        )

      )

    )



    testthat::expect_error(

      read_excel_allsheets(

        path,

        skip = 5

      ),

      NA

    )

  })



testthat::test_that(
  "verbose mode executes successfully",
  {

    path <- simple_workbook()

    testthat::expect_error(

      read_excel_allsheets(

        path,

        verbose = TRUE

      ),

      NA

    )

  })

# =============================================================================
# ENGINE CONSISTENCY
# =============================================================================

testthat::test_that(
  "readxl and openxlsx discover the same worksheets",
  {

    path <- simple_workbook()

    readxl_book <- read_excel_allsheets(

      path,

      engine = "readxl"

    )



    openxlsx_book <- read_excel_allsheets(

      path,

      engine = "openxlsx"

    )



    testthat::expect_equal(

      names(readxl_book$sheets),

      names(openxlsx_book$sheets)

    )

  })



testthat::test_that(
  "engine metadata records selected backend",
  {

    path <- simple_workbook()

    book <- read_excel_allsheets(

      path,

      engine = "openxlsx"

    )



    testthat::expect_equal(

      book$metadata$engine,

      "openxlsx"

    )

  })

# =============================================================================
# WARNINGS AND ERROR HANDLING
# =============================================================================

testthat::test_that(
  "parallel argument is currently accepted",
  {

    path <- simple_workbook()

    testthat::expect_error(

      read_excel_allsheets(

        path,

        parallel = FALSE

      ),

      NA

    )

  })



testthat::test_that(
  "return_failed argument is currently accepted",
  {

    path <- simple_workbook()

    testthat::expect_error(

      read_excel_allsheets(

        path,

        return_failed = FALSE

      ),

      NA

    )

  })



testthat::test_that(
  "fails gracefully for corrupted workbooks",
  {

    corrupt <- make_corrupt_excel_file()

    testthat::expect_error(

      read_excel_allsheets(

        corrupt

      ),

      "Unable to read workbook structure"

    )

  })


# =============================================================================
# PUBLIC API
# =============================================================================

testthat::test_that(
  "get_sheet returns worksheet by name",
  {

    path <- simple_workbook()

    book <- read_excel_allsheets(
      path
    )

    sheet <- get_sheet(
      book,
      "A"
    )

    testthat::expect_s3_class(
      sheet,
      "excel_sheet"
    )

  })



testthat::test_that(
  "get_sheet returns worksheet by numeric index",
  {

    path <- simple_workbook()

    book <- read_excel_allsheets(
      path
    )

    sheet <- get_sheet(
      book,
      2
    )

    testthat::expect_s3_class(
      sheet,
      "excel_sheet"
    )

  })



testthat::test_that(
  "get_sheet warns when worksheet does not exist",
  {

    path <- simple_workbook()

    book <- read_excel_allsheets(
      path
    )

    testthat::expect_warning(

      result <- get_sheet(
        book,
        "UNKNOWN"
      ),

      "Sheet not found"

    )



    testthat::expect_null(
      result
    )

  })



testthat::test_that(
  "get_sheet rejects invalid indices",
  {

    path <- simple_workbook()

    book <- read_excel_allsheets(
      path
    )



    testthat::expect_error(

      get_sheet(
        book,
        0
      ),

      "Sheet index out of bounds"

    )



    testthat::expect_error(

      get_sheet(
        book,
        10
      ),

      "Sheet index out of bounds"

    )

  })



testthat::test_that(
  "sheet_names returns worksheet names",
  {

    path <- simple_workbook()

    book <- read_excel_allsheets(
      path
    )



    testthat::expect_equal(

      sheet_names(book),

      c(
        "A",
        "B",
        "C"
      )

    )

  })



testthat::test_that(
  "sheet_names rejects invalid objects",
  {

    testthat::expect_error(

      sheet_names(
        data.frame()
      ),

      "excel_book"

    )

  })

# =============================================================================
# EDGE CASES
# =============================================================================

testthat::test_that(
  "imports workbook containing an empty worksheet",
  {

    path <- make_excel_file(

      list(

        Empty = data.frame()

      )

    )



    book <- read_excel_allsheets(
      path
    )



    testthat::expect_s3_class(
      book,
      "excel_book"
    )



    testthat::expect_equal(

      names(book$sheets),

      "Empty"

    )

  })



testthat::test_that(
  "imports workbook containing unicode worksheet names",
  {

    path <- make_excel_file(

      list(

        "Sinistros_🚗" = data.frame(
          x = 1:3
        ),

        "Prémios_çãõ" = data.frame(
          y = 1:3
        )

      )

    )



    book <- read_excel_allsheets(
      path
    )



    testthat::expect_equal(

      sheet_names(book),

      c(
        "Sinistros_🚗",
        "Prémios_çãõ"
      )

    )

  })



testthat::test_that(
  "imports workbook containing very long worksheet names",
  {

    nm <- paste0(

      rep(
        "A",
        31
      ),

      collapse=""

    )



    path <- make_excel_file(

      stats::setNames(

        list(

          data.frame(
            x=1
          )

        ),

        nm

      )

    )



    book <- read_excel_allsheets(
      path
    )



    testthat::expect_equal(

      sheet_names(book),

      nm

    )

  })



testthat::test_that(
  "imports workbook containing a large number of worksheets",
  {

    sheets <- lapply(

      seq_len(40),

      function(i){

        data.frame(
          id=i
        )

      }

    )



    names(sheets) <-

      paste0(
        "Sheet",
        seq_len(40)
      )



    path <- make_excel_file(
      sheets
    )



    book <- read_excel_allsheets(
      path
    )



    testthat::expect_length(

      book$sheets,

      40

    )

  })



testthat::test_that(
  "imports workbook containing heterogeneous column types",
  {

    path <- make_excel_file(

      list(

        Portfolio=data.frame(

          integer=1:3,

          numeric=c(
            1.1,
            2.2,
            3.3
          ),

          character=c(
            "A",
            "B",
            "C"
          ),

          logical=c(
            TRUE,
            FALSE,
            TRUE
          ),

          stringsAsFactors=FALSE

        )

      )

    )



    book <- read_excel_allsheets(
      path
    )



    testthat::expect_s3_class(

      book,

      "excel_book"

    )

  })

