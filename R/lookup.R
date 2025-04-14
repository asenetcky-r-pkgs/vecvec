#' @title Prepare Lookup Tibble
#' @description Standardizes the lookup tibble.
#' @param lookup A lookup tibble.
#' @returns A prepped lookup tibble.
prep_lookup <- function(lookup) {
  original_row_id <- NULL

  prep_stnd_string(lookup) |>
    dplyr::rename(lookup_id = original_row_id) |>
    dplyr::select(-"unique_string_id")
}
