#' read csv in a data frame
#'
#' @param file A string, file name.
#' @param sep A character, separator.
#'
#' @return A data frame
#'
#' @family read functions
#'
#' @export
read_csv <- function(file, sep = ',') {
  ft <- readr::read_delim(
    file,
    delim = sep,
    # locale = readr::locale(encoding = 'latin1'),
    col_types = readr::cols(.default = readr::col_character())
  )
  ft[, names(ft)] <-
    apply(ft[, names(ft), drop = FALSE], 2, function(x)
      tidyr::replace_na(x, ""))
  ft
}


#' read Excel in a data frame
#'
#' @param file A string, file name.
#' @param sheet_index A number, sheet number.
#' @param sheet_name A string, sheet name
#'
#' @return A data frame
#'
#' @family read functions
#'
#' @export
read_excel <- function(file, sheet_index = NULL, sheet_name = NULL) {
  if (is.null(sheet_index) & is.null(sheet_name)) {
    sheet_name <- readxl::excel_sheets(file)
  } else if (is.null(sheet_name)) {
    sheet_name <- readxl::excel_sheets(file)[sheet_index]
  }
  sheet_name <- sheet_name[1]
  ft <- suppressMessages(
    readxl::read_excel(
      file,
      sheet = sheet_name,
      col_names = TRUE,
      col_types = "text",
      trim_ws = TRUE
    )
  )
  ft[, names(ft)] <-
    apply(ft[, names(ft), drop = FALSE], 2, function(x)
      tidyr::replace_na(x, ""))
  ft
}

