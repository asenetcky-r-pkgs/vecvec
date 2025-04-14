#' @title Grab Columns
#' @description Extracts specific columns from a data frame.
#' @param x A data frame.
#' @return A data frame with selected columns.
#' @importFrom dplyr select
grab_cols <- function(x) {
  unique_string_id <- lookup_id <- NULL

  x |>
    dplyr::select(
      unique_string_id,
      lookup_id
    )
}
