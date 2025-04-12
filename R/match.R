exact_whole_match <- function(.data, lookup) {
  .data |>
    dplyr::inner_join(
      lookup,
      by = "exact_string"
    ) |>
    grab_cols()
}

format_as_tidytext <- function(.data) {
  unique_string_id <- prep_string <- prep_string_token <- NULL

  .data |>
    dplyr::select("unique_string_id", "prep_string") |>
    tidytext::unnest_tokens(prep_string_token, prep_string) |>
    dplyr::filter(
      # test this out
      stringr::str_length(prep_string_token) > 1
    )
}

cross_join <- function(.data, lookup) {
  dplyr::cross_join(.data, lookup)
}

exact_token_match <- function(.data) {
  prep_string_token <- lookup_string <- NULL

  .data |>
    dplyr::filter(prep_string_token == prep_string) |>
    grab_cols()
}

exact_substring_match <- function(.data) {
  prep_string_token <- lookup_string <- NULL

  .data |>
    dplyr::filter(
      stringr::str_detect(prep_string_token, prep_string)
    ) |>
    grab_cols()
}

fuzzy_match <- function(primary, lookup, dist = 1) {
  prep_string_token <- lookup_id <- NULL
  fuzzy_target <- dplyr::pull(lookup, prep_string)

  primary |>
    dplyr::mutate(
      lookup_id = stringdist::amatch(
        prep_string_token,
        fuzzy_target,
        maxDist = dist
      )
    ) |>
    dplyr::filter(!is.na(lookup_id)) |>
    grab_cols()
}

tidy_matches <- function(primary, lookup) {
  lookup_id <- exact_string <- NULL

  tidy <- format_as_tidytext(primary)

  lookup <-
    lookup |>
    dplyr::select(lookup_id, prep_string, exact_string)
  cross_joined <- cross_join(tidy, lookup)

  dplyr::lst(
    exact_tokens = exact_token_match(cross_joined),
    exact_substring = exact_substring_match(cross_joined),
    fuzzies = fuzzy_match(tidy, lookup)
  )
}

match <- function(primary, lookup) {
  checkmate::assert(
    checkmate::check_data_frame(primary),
    checkmate::check_names(
      colnames(primary),
      must.include = c(
        "stnd_string",
        "prep_string",
        "exact_string"
      )
    ),
    combine = "and"
  )

  dplyr::lst(
    exact_whole_string = exact_whole_match(primary, lookup),
    tidy = tidy_matches(primary, lookup)
  ) |>
    purrr::list_flatten()
}
