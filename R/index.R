#' @title Standardize Vector
#' @description Standardizes a character vector for indexing.
#' @param vec A character vector.
#' @returns A tibble with standardized strings.
standardize_vec <- function(vec) {
  value <- stnd_string <- original_row_id <- NULL

  tibble::as_tibble_col(vec) |>
    dplyr::mutate(
      original_row_id = dplyr::row_number(),
      stnd_string = value,
      stnd_string = standardize_str(stnd_string),
      stnd_string = dplyr::if_else(
        stnd_string %in% c("_", "", "na", "null", "-"),
        true = NA_character_,
        false = stnd_string
      )
    ) |>
    dplyr::relocate(original_row_id)
}

#' @title Reduce Standardized Strings
#' @description Reduces a data frame to unique standardized strings.
#' @param .data A data frame of standardized strings.
#' @returns A data frame of unique strings.
reduce_stnd_strings <- function(.data) {
  stnd_string <- NULL

  # grab only unique strings
  .data |>
    dplyr::filter(!is.na(stnd_string)) |>
    dplyr::pull(stnd_string) |>
    list
  unique() |>
    tibble::as_tibble_col("stnd_string") |>
    dplyr::mutate(unique_string_id = dplyr::row_number())
}

#' @title Map Indicies to Data
#' @description Maps unique string IDs back to the original data.
#' @param .data A data frame of indices.
#' @param unique_strings A data frame of unique strings.
#' @returns A data frame with mapped IDs.
map_ids <- function(.data, unique_strings) {
  checkmate::assert(
    checkmate::check_data_frame(unique_strings),
    checkmate::check_true("stnd_string" %in% colnames(unique_strings)),
    combine = "and"
  )

  .data |>
    dplyr::inner_join(
      unique_strings,
      by = "stnd_string"
    )
}

#' @title Index
#' @description Creates an index for a character vector.
#' @param vec A character vector.
#' @returns A list containing data and string indices.
index <- function(vec) {
  unique_string_id <- original_row_id <- NULL

  checkmate::assert(
    checkmate::check_vector(vec, strict = TRUE, min.len = 1L),
    checkmate::check_character(vec),
    combine = "and"
  )

  index <- standardize_vec(vec)

  unique_strings <- reduce_stnd_strings(index)

  index <-
    map_ids(index, unique_strings) |>
    dplyr::relocate(
      unique_string_id,
      .after = original_row_id
    )

  # bundle it up
  dplyr::lst(
    data_index = index,
    string_index = unique_strings
  )
}
