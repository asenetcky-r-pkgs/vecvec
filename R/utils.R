grab_cols <- function(x) {
  unique_string_id <- lookup_id <- NULL

  x |>
    dplyr::select(
      unique_string_id,
      lookup_id
    )
}
