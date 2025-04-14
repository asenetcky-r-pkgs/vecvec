#' @title Standardize String
#' @description Converts a string to lowercase and removes extra spaces.
#' @param x A character vector.
#' @returns A standardized character vector.
standardize_str <- function(x) {
  stringr::str_to_lower(
    stringr::str_squish(
      x
    )
  )
}

#' @title Replace Separators
#' @description Replaces separators (e.g., commas, underscores) with spaces.
#' @param x A character vector.
#' @returns A character vector with separators replaced.
replace_sep <- function(x) {
  stringr::str_replace_all(
    x,
    pattern = "[,\\._\\-&#]",
    replacement = " "
  ) |>
    stringr::str_squish()
}

#' @title Remove Slash
#' @description Removes slashes from a character vector.
#' @param x A character vector.
#' @returns A character vector without slashes.
remove_slash <- function(x) {
  stringr::str_remove_all(
    x,
    pattern = "/"
  ) |>
    stringr::str_squish()
}

#' @title Remove Numbers
#' @description Removes numbers from a character vector.
#' @param x A character vector.
#' @returns A character vector without numbers.
remove_numbers <- function(x) {
  stringr::str_remove_all(
    x,
    pattern = "\\d"
  ) |>
    stringr::str_squish()
}

#' @title Remove All Spaces
#' @description Removes all spaces from a character vector.
#' @param x A character vector.
#' @returns A character vector without spaces.
remove_all_spaces <- function(x) {
  stringr::str_remove_all(x, " ")
}

#' @title Prepare String
#' @description Prepares a string by replacing separators, removing slashes, and removing numbers.
#' @param x A character vector.
#' @returns A prepared character vector.
prep_string <- function(x) {
  checkmate::check_character(x)
  x |>
    replace_sep() |>
    remove_slash() |>
    remove_numbers()
}

#' @title Prepare Standardized String
#' @description Prepares a standardized string for matching.
#' @param .data A data frame containing a `stnd_string` column.
#' @returns A data frame with prepared strings.
prep_stnd_string <- function(.data) {
  stnd_string <- NULL

  checkmate::assert(
    checkmate::check_data_frame(.data),
    checkmate::check_names(
      colnames(.data),
      must.include = "stnd_string"
    ),
    combine = "and"
  )

  .data |>
    dplyr::mutate(
      prep_string = prep_string(stnd_string),
      exact_string = remove_all_spaces(prep_string)
    )
}
