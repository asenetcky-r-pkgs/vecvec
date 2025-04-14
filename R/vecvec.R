vecvec <- function(primary, lookup) {
  checkmate::assert(
    checkmate::check_character(
      primary,
      min.chars = 2,
      any.missing = FALSE,
      all.missing = FALSE
    ),
    checkmate::check_character(
      lookup,
      min.chars = 2,
      any.missing = FALSE,
      all.missing = FALSE
    ),
    combine = "and"
  )

  # standardize and index
  primary_index <- index(primary)
  lookup_index <- index(lookup)

  # prep
  prepped_primary <- prep_stnd_string(primary_index$string_index)
  prepped_lookup <- prep_lookup(lookup_index$data_index)

  # match
  matches <- match(prepped_primary, prepped_lookup)

  #vecvec
  dplyr::lst(
    indices = dplyr::lst(
      primary = primary_index,
      lookup = lookup_index
    ),
    matches = matches
  )
}

create_match_matrix <- function(.vecvec) {
  match_matrix <-
    .vecvec$indices$primary$data_index |>
    dplyr::select(
      original_row_id,
      unique_string_id,
      primary = value
    )

  # TODO: implement some purrr magic + mini functions
  ewhlstr <-
    .vecvec$matches$exact_whole_string |>
    dplyr::rename(exact_whole_string = lookup_id)

  etok <-
    .vecvec$matches$tidy_exact_tokens |>
    dplyr::rename(exact_token = lookup_id)

  esubstr <-
    .vecvec$matches$tidy_exact_substring |>
    dplyr::rename(exact_substring = lookup_id)

  fuzz <-
    .vecvec$matches$tidy_fuzzies |>
    dplyr::rename(fuzzy = lookup_id)

  r <- "many-to-many"
  match_matrix |>
    dplyr::left_join(ewhlstr, by = "unique_string_id", relationship = r) |>
    dplyr::left_join(etok, by = "unique_string_id", relationship = r) |>
    dplyr::left_join(esubstr, by = "unique_string_id", relationship = r) |>
    dplyr::left_join(fuzz, by = "unique_string_id", relationship = r)
}

unique_matches <- function(match_matrix) {
  match_matrix |>
    tidyr::pivot_longer(
      cols = c(exact_whole_string, exact_token, exact_substring, fuzzy),
      names_to = "match_type",
      values_to = "lookup_id"
    ) |>
    dplyr::group_by(original_row_id, primary) |>
    dplyr::reframe(lookup_id = unique(lookup_id)) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(lookup_id))
}

human_readable <- function(.vecvec) {
  lookup <-
    .vecvec$indices$lookup$data_index |>
    dplyr::select(
      lookup_id = original_row_id,
      lookup_value = value
    )

  .vecvec |>
    unique_matches() |>
    create_match_matrix() |>
    dplyr::left_join(
      lookup,
      by = "lookup_id"
    )
}
