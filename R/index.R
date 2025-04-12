standardize_vec <- function(vec) {
  tibble::as_tibble_col(var) |>
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

reduce_stnd_strings <- function(.data) {
  # grab only unique strings
  .data |>
    dplyr::filter(!is.na(stnd_string)) |>
    dplyr::pull(stnd_string) |>
    unique() |>
    tibble::as_tibble_col("stnd_string") |>
    dplyr::mutate(unique_string_id = dplyr::row_number())
}

backflush_ids <- function(.data, unique_strings) {
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

# return named list of indices
index <- function(var, name) {
  checkmate::assert(
    checkmate::check_vector(var, strict = TRUE, min.len = 1L),
    checkmate::check_character(var),
    checkmate::check_character(name, min.chars = 1L),
    combine = "and"
  )

  index <- standardize_var(var, name)

  unique_strings <- reduce_stnd_strings(index)

  index <-
    backflush_ids(index, unique_strings) |>
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
