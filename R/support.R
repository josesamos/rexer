
#' Transforms a vector of strings into a string
#'
#' Insert the separator ("<|>") to later facilitate the reverse operation.
#'
#' @param vector A vector of strings.
#'
#' @return A string.
#'
#' @family support functions
#'
#' @examples
#'
#' s <- vector_to_string(c('Addition', '+'))
#'
#' @export
vector_to_string <- function(vector) {
  if (is.null(vector)) {
    res <- ""
  } else {
    res <- paste(vector, collapse = "<|>")
  }
  res
}

#' Set pending answers
#'
#' The answers we want to process are indicated by the character "?". Fill in the
#' specified answers by generating combinations of the available options to fill
#' the gaps in order, from value 1 to the maximum number of options available: the
#' first option for each of the gaps, the second for each of the gaps, and so on.
#'
#' @param df A data frame of exercises.
#'
#' @return A data frame.
#'
#' @family support functions
#'
#' @examples
#'
#' file <- system.file("extdata/example.csv", package = "rexer")
#' df <- read_exercise_csv(file)
#' df <- set_pending_answers(df)
#'
#' @export
set_pending_answers <- function(df) {
  attributes <- names(df)
  rest <-
    setdiff(attributes,
            c("type", "statement", "image", "image_alt", "answer"))
  for (i in 1:nrow(df)) {
    l <- list()
    if (df$answer[i] == '?') {
      for (j in rest) {
        if (df[i, j][[1]] != "") {
          l[[j]] <- string_to_vector(df[i, j][[1]])
        }
      }
    }
    if (length(l) > 0) {
      cycle <- max(unlist(lapply(l, length), use.names = FALSE))
      val <- NULL
      for (k in 1:cycle) {
        for (j in seq_along(names(l))) {
          m <- length(l[[j]])
          n <- k %% m
          if (n == 0) {
            n <- m
          }
          if (j > 1) {
            val <- paste0(val, ', ')
          }
          val <- paste0(val, j, '.', l[[j]][n])
        }
        if (k < cycle) {
          val <- paste0(val, '<|>')
        }
      }
      df$answer[i] <- val
    }
  }
  df
}

#' Write an exercise csv file
#'
#' Writes an exercise data frame in a csv file of exercises.
#'
#' @param df A data frame.
#' @param file A string, name of a text file.
#' @param sep Column separator character.
#'
#' @return A string.
#'
#' @family support functions
#'
#' @examples
#'
#' file <- system.file("extdata/example.csv", package = "rexer")
#' df <- read_exercise_csv(file)
#' df <- set_pending_answers(df)
#'
#' write_exercise_csv(df, file = tempfile(fileext = '.csv'))
#'
#' @export
write_exercise_csv <- function(df, file, sep = ',') {
  if (sep == ',') {
    utils::write.csv(df, file = file, row.names = FALSE)
  } else {
    utils::write.csv2(df, file = file, row.names = FALSE)
  }
  invisible(file)
}


#' Create an exercise data frame
#'
#' Creates an empty exercise data frame.
#'
#' @return A data frame.
#'
#' @family support functions
#'
#' @examples
#'
#' df <- create_exercise_data_frame()
#'
#' @export
create_exercise_data_frame <- function() {
  exercises <-  data.frame(
    type = character(),
    statement = character(),
    image = character(),
    image_alt = character(),
    answer = character(),
    a_1 = character(),
    a_2 = character(),
    a_3 = character(),
    stringsAsFactors = FALSE
  )
  exercises
}


#' Create an exercise csv file
#'
#' Creates an empty exercise csv file.
#'
#' @param file A string, name of a text file.
#' @param sep Column separator character.
#'
#' @return A string.
#'
#' @family support functions
#'
#' @examples
#'
#' file <- create_exercise_csv(file = tempfile(fileext = '.csv'))
#'
#' @export
create_exercise_csv <- function(file, sep = ',') {
  exercises <- create_exercise_data_frame()
  if (sep == ',') {
    utils::write.csv(exercises, file = file, row.names = FALSE)
  } else {
    utils::write.csv2(exercises, file = file, row.names = FALSE)
  }
  invisible(file)
}


#' Read an exercise csv file
#'
#' Reads a csv file of exercises and returns a data frame.
#'
#' @param file A string, name of a text file.
#' @param sep Column separator character.
#'
#' @return A data frame.
#'
#' @family support functions
#'
#' @examples
#'
#' file <- system.file("extdata/exercises.csv", package = "rexer")
#' df <- read_exercise_csv(file)
#'
#' @export
read_exercise_csv <- function(file, sep = ',') {
  df <- readr::read_delim(
    file,
    delim = sep,
    col_types = readr::cols(.default = readr::col_character())
  )
  attributes <- names(df)
  df[, attributes] <- data.frame(lapply(df[, attributes], as.character), stringsAsFactors = FALSE)
  if (nrow(df) == 1) {
    df[, attributes] <-
      tibble::as_tibble(as.list(apply(df[, attributes, drop = FALSE], 2, function(x)
        tidyr::replace_na(x, ''))))
  } else {
    df[, attributes] <-
      apply(df[, attributes, drop = FALSE], 2, function(x)
        tidyr::replace_na(x, ''))
  }
  attributes <- snakecase::to_snake_case(attributes)
  names(df) <- attributes
  df
}


#' Create an exercise Excel file
#'
#' Creates an empty exercise Excel file.
#'
#' @param file A string, name of a text file.
#'
#' @return A string.
#'
#' @family support functions
#'
#' @examples
#'
#' \donttest{
#' file <- create_exercise_excel(file = tempfile(fileext = '.xlsx'))
#' }
#'
#' @export
create_exercise_excel <- function(file) {
  exercises <- create_exercise_data_frame()
  xlsx::write.xlsx(
    as.data.frame(exercises),
    file = file,
    sheetName = 'moodef',
    row.names = FALSE,
    showNA = FALSE
  )
  invisible(file)
}


#' Read an exercise Excel file
#'
#' Reads an Excel file of exercises and returns a data frame.
#'
#' In addition to the file, we can indicate the sheet by its name or index. If we
#' do not indicate anything, it considers the first sheet.
#'
#' @param file A string, name of a text file.
#' @param sheet_index A number, sheet index in the workbook.
#' @param sheet_name A string, sheet name.
#'
#' @return A data frame.
#'
#' @family support functions
#'
#' @examples
#'
#' file <- system.file("extdata/exercises.xlsx", package = "rexer")
#' df <- read_exercise_excel(file)
#'
#' @export
read_exercise_excel <- function(file,
                                sheet_index = NULL,
                                sheet_name = NULL) {
  if (is.null(sheet_index) & is.null(sheet_name)) {
    sheet_name <- readxl::excel_sheets(file)
  } else if (is.null(sheet_name)) {
    sheet_name <- readxl::excel_sheets(file)[sheet_index]
  }
  sheet_name <- sheet_name[1]
  df <- suppressMessages(
    readxl::read_excel(
      file,
      sheet = sheet_name,
      col_names = TRUE,
      col_types = "text",
      trim_ws = TRUE
    )
  )
  attributes <- names(df)
  df[, attributes] <-
    data.frame(lapply(df[, attributes], as.character), stringsAsFactors = FALSE)
  if (nrow(df) == 1) {
    df[, attributes] <-
      tibble::as_tibble(as.list(apply(df[, attributes, drop = FALSE], 2, function(x)
        tidyr::replace_na(x, ''))))
  } else {
    df[, attributes] <-
      apply(df[, attributes, drop = FALSE], 2, function(x)
        tidyr::replace_na(x, ''))
  }
  attributes <- snakecase::to_snake_case(attributes)
  names(df) <- attributes
  df
}
