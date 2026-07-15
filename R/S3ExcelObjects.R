# ============================================================================
# S3 Excel Objects
# ============================================================================
#
# Defines the core S3 classes used by xlversion:
#
# - excel_file : physical Excel file representation
# - excel_book : imported workbook container
# - excel_sheet: imported worksheet representation
#
# These objects provide a stable internal contract for the package.
#
# ============================================================================





# ============================================================================
# excel_file
# ============================================================================



#' Create a New excel_file Object
#'
#' Internal constructor for an `excel_file` S3 object.
#'
#' The object represents the physical Excel workbook stored on disk,
#' independently from its imported content.
#'
#' @param filename Character scalar. Path to the Excel file.
#'
#' @return An object of class `excel_file`.
#'
#' @keywords internal
new_excel_file <- function(filename) {


  stopifnot(

    "filename must be a character value" =
      is.character(filename) &&
      length(filename)==1

  )



  if(!file.exists(filename)){

    stop(
      "Excel file does not exist: ",
      filename,
      call.=FALSE
    )

  }



  info <- file.info(filename)



  structure(

    list(

      filename =
        normalizePath(filename, mustWork = TRUE),


      size =
        info$size,


      modified =
        info$mtime,


      loaded_at =
        Sys.time()

    ),

    class="excel_file"

  )

}





#' Print excel_file Object
#'
#' Displays a compact summary of an Excel file.
#'
#' @param x An object of class `excel_file`.
#' @param ... Additional arguments.
#'
#' @export
print.excel_file <- function(x,...){


  cat("<excel_file>\n")


  cat(
    "File:",
    basename(x$filename),
    "\n"
  )


  cat(
    "Size:",
    format(
      x$size,
      big.mark=","
    ),
    "bytes\n"
  )


  cat(
    "Modified:",
    as.character(x$modified),
    "\n"
  )



  invisible(x)

}





#' Get File Path from excel_file
#'
#' Extracts the physical file location.
#'
#' @param x An object of class `excel_file`.
#'
#' @return Character scalar containing the file path.
#'
#' @export
get_filename <- function(x){


  if(!inherits(x,"excel_file")){

    stop(
      "`x` must be an excel_file object.",
      call.=FALSE
    )

  }


  x$filename

}





# ============================================================================
# excel_book
# ============================================================================





#' Create a New excel_book Object
#'
#' Internal constructor for an `excel_book` S3 object.
#'
#' An `excel_book` contains imported worksheets and failed worksheet
#' information.
#'
#' @param sheets Named list of `excel_sheet` objects.
#' @param failed_sheets Named list of failed worksheet objects.
#' @param file Optional `excel_file` object.
#'
#' @return An object of class `excel_book`.
#'
#' @keywords internal
new_excel_book <- function(
    sheets=list(),
    failed_sheets=list(),
    file=NULL
){


  stopifnot(
    is.list(sheets),
    is.list(failed_sheets)
  )



  if(length(sheets)>0 &&
     is.null(names(sheets))){

    names(sheets) <-
      paste0(
        "sheet",
        seq_along(sheets)
      )

  }



  structure(

    list(

      sheets=sheets,

      failed_sheets=failed_sheets,

      file=file,

      metadata=list(

        created_at=Sys.time(),

        n_sheets=
          length(sheets),

        n_failed=
          length(failed_sheets)

      )

    ),

    class="excel_book"

  )

}





#' Print excel_book Object
#'
#' Displays workbook summary information.
#'
#' @param x An object of class `excel_book`.
#' @param ... Additional arguments.
#'
#' @export
print.excel_book <- function(x,...){


  cat("<excel_book>\n")



  if(!is.null(x$file)){

    cat(
      "Source:",
      basename(
        x$file$filename
      ),
      "\n"
    )

  }



  cat(
    "Sheets:",
    length(x$sheets),
    "\n"
  )



  if(length(x$sheets)>0){

    cat(
      "Names:",
      paste(
        names(x$sheets),
        collapse=", "
      ),
      "\n"
    )

  }



  cat(
    "Failed:",
    length(x$failed_sheets),
    "\n"
  )



  invisible(x)

}





#' Extract Sheet from excel_book
#'
#' Returns a worksheet from an imported workbook.
#'
#' @param book An `excel_book` object.
#' @param name Character scalar or numeric position.
#'
#' @return An `excel_sheet` object or NULL.
#'
#' @export
get_sheet <- function(
    book,
    name
){


  if(!inherits(book,"excel_book")){

    stop(
      "`book` must be an excel_book object.",
      call.=FALSE
    )

  }



  if(is.numeric(name)){


    if(name < 1 ||
       name > length(book$sheets)){

      stop(
        "Sheet index out of bounds.",
        call.=FALSE
      )

    }


    name <-
      names(book$sheets)[name]

  }



  if(!name %in% names(book$sheets)){

    warning(
      "Sheet not found: ",
      name,
      call.=FALSE
    )

    return(NULL)

  }



  book$sheets[[name]]

}





#' List Sheets in excel_book
#'
#' Returns available worksheet names.
#'
#' @param x An `excel_book` object.
#'
#' @return Character vector with sheet names.
#'
#' @export
sheet_names <- function(x){

  if (!inherits(x, "excel_book")) {

    stop(
      "`x` must be an excel_book object.",
      call. = FALSE
    )

  }

  names <- names(x$sheets)

  if (is.null(names)) {

    character(0)

  } else {

    names

  }

}

# ============================================================================
# excel_sheet
# ============================================================================

#' Create a New excel_sheet Object
#'
#' Internal constructor for an `excel_sheet` S3 object.
#'
#' An `excel_sheet` represents a single worksheet imported from an
#' Excel workbook together with basic metadata describing its origin.
#'
#' @param data A data.frame or tibble.
#' @param sheet Character scalar. Worksheet name.
#' @param source Character scalar. Excel file path.
#'
#' @return An object of class `excel_sheet`.
#'
#' @keywords internal
new_excel_sheet <- function(
    data,
    sheet,
    source
){

  stopifnot(

    "data must be a data.frame or tibble" =
      inherits(data, "data.frame"),

    "sheet must be a character scalar" =
      is.character(sheet) &&
      length(sheet) == 1,

    "source must be a character scalar" =
      is.character(source) &&
      length(source) == 1

  )


  structure(

    list(

      data = data,

      sheet = sheet,

      source = normalizePath(
        source,
        mustWork = TRUE
      ),

      n_rows = nrow(data),

      n_cols = ncol(data),

      imported_at = Sys.time()

    ),

    class = "excel_sheet"

  )

}

#' Print excel_sheet Object
#'
#' @param x An excel_sheet object.
#' @param ... Additional arguments.
#'
#' @export
print.excel_sheet <- function(x, ...){

  cat("<excel_sheet>\n")

  cat(
    "Sheet:",
    x$sheet,
    "\n"
  )

  cat(
    "Rows:",
    x$n_rows,
    "\n"
  )

  cat(
    "Columns:",
    x$n_cols,
    "\n"
  )

  invisible(x)

}

#' Coerce excel_sheet to data.frame
#'
#' @param x An excel_sheet object.
#' @param ... Additional arguments.
#'
#' @export
as.data.frame.excel_sheet <- function(
    x,
    ...
){

  x$data

}

#' Number of Rows in an excel_sheet
#'
#' @param x An excel_sheet object.
#'
#' @return Integer.
#'
#' @export
nrow.excel_sheet <- function(x){

  x$n_rows

}


#' Number of Columns in an excel_sheet
#'
#' @param x An excel_sheet object.
#'
#' @return Integer.
#'
#' @export
ncol.excel_sheet <- function(x){

  x$n_cols

}


#' Names of data
#'
#' @param x An excel_sheet object.
#'
#' @return Character vector
#'
#' @export
names.excel_sheet <- function(x){

  names(x$data)

}

#' Extract Data from an excel_sheet
#'
#' Returns the imported data stored inside an `excel_sheet` object.
#'
#' @param x An `excel_sheet` object.
#'
#' @return A data.frame or tibble.
#'
#' @export
get_data <- function(x){

  if(!inherits(x, "excel_sheet")){

    stop(
      "`x` must be an excel_sheet object.",
      call. = FALSE
    )

  }

  x$data

}





