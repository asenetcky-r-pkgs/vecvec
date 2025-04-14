#' @title Match Primary Vector Against Lookup Vector
#' @description Matches a primary character vector against a lookup vector and returns indices and matches.
#'
#' @param primary A character vector to be matched.
#' @param lookup A character vector to match against.
#'
#' @export
#'
#' @returns A list containing indices and matches.
#'
#' @examples
#' vecvec(create_random_names(), create_random_names())
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

#' @title Create Match Matrix
#' @description Creates a match matrix from a `vecvec` object.
#' @param .vecvec A `vecvec` object.
#' @returns A data frame containing the match matrix.
create_match_matrix <- function(.vecvec) {
  value <- lookup_id <- NULL

  match_matrix <-
    .vecvec$indices$primary$data_index |>
    dplyr::select(
      "original_row_id",
      "unique_string_id",
      primary = value
    )

  # TODO: implement some purrr magic + mini functions
  whole <-
    .vecvec$matches$exact_whole_string |>
    dplyr::rename(whole = lookup_id)

  token <-
    .vecvec$matches$tidy_exact_tokens |>
    dplyr::rename(token = lookup_id)

  substring <-
    .vecvec$matches$tidy_exact_substring |>
    dplyr::rename(substring = lookup_id)

  fuzzy <-
    .vecvec$matches$tidy_fuzzies |>
    dplyr::rename(fuzzy = lookup_id)

  r <- "many-to-many"
  match_matrix |>
    dplyr::left_join(whole, by = "unique_string_id", relationship = r) |>
    dplyr::left_join(token, by = "unique_string_id", relationship = r) |>
    dplyr::left_join(substring, by = "unique_string_id", relationship = r) |>
    dplyr::left_join(fuzzy, by = "unique_string_id", relationship = r)
}

#' @title Extract Unique Matches
#' @description Extracts unique matches from a match matrix.
#' @param match_matrix A match matrix.
#' @param match_vars A character vector of match variable names.
#' @returns A data frame of unique matches.
unique_matches <- function(
  match_matrix,
  match_vars = c(
    "whole",
    "token",
    "substring",
    "fuzzy"
  )
) {
  primary <- lookup_id <- original_row_id <- NULL

  match_matrix |>
    tidyr::pivot_longer(
      cols = {{ match_vars }},
      names_to = "match_type",
      values_to = "lookup_id"
    ) |>
    dplyr::group_by(original_row_id, primary) |>
    dplyr::reframe(lookup_id = unique(lookup_id)) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(lookup_id))
}

#' @title Human-Readable Matches
#' @description Converts a `vecvec` object into a human-readable format.
#'
#' @param .vecvec A `vecvec` object.
#' @param match_vars A character vector of specifying which matches to return.
#' Defaults to all matches:
#'
#' * whole: exact whole word matches with all spaces stripped out
#' * token: exact whole token matches
#' * substring: exact substring match
#' * fuzzy: fuzzy text match using OSA - strict, max distance == 1
#'
#' @returns A data frame of human-readable matches.
#'
#' @export
#'
#' @examples
#' vecvec(create_random_names(), create_random_names()) |> human_readable()
human_readable <- function(
  .vecvec,
  match_vars = c(
    "whole",
    "token",
    "substring",
    "fuzzy"
  )
) {
  lookup <-
    .vecvec$indices$lookup$data_index |>
    dplyr::select(
      lookup_id = "original_row_id",
      lookup_value = "value"
    )

  perm_vars <-
    c(
      "original_row_id",
      "unique_string_id",
      "primary"
    )

  .vecvec |>
    create_match_matrix() |>
    dplyr::select(dplyr::all_of(c(perm_vars, match_vars))) |>
    unique_matches(match_vars) |>
    dplyr::left_join(
      lookup,
      by = "lookup_id"
    )
}
