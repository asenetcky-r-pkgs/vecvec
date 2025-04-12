standardize_str <- function(x) {
  stringr::str_to_lower(
    stringr::str_squish(
      x
    )
  )
}

replace_sep <- function(x) {
  stringr::str_replace_all(
    x,
    pattern = "[,\\._\\-&#]",
    replacement = " "
  ) |>
    stringr::str_squish()
}

remove_slash <- function(x) {
  stringr::str_remove_all(
    x,
    pattern = "/"
  ) |>
    stringr::str_squish()
}

remove_numbers <- function(x) {
  stringr::str_remove_all(
    x,
    pattern = "\\d"
  ) |>
    stringr::str_squish()
}

remove_all_spaces <- function(x) {
  stringr::str_remove_all(x, " ")
}

prep_string <- function(x) {
  checkmate::check_character(x)
  x |>
    replace_sep() |>
    remove_slash() |>
    remove_numbers()
}

prep_stnd_string <- function(.data) {
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
