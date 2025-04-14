#' @title Grab Columns
#' @description Extracts specific columns from a data frame.
#' @param x A data frame.
#' @returns A data frame with selected columns.
grab_cols <- function(x) {
  unique_string_id <- lookup_id <- NULL

  x |>
    dplyr::select(
      unique_string_id,
      lookup_id
    )
}
