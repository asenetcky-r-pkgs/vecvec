#' @title Exact Whole Match
#' @description Performs an exact match of whole strings between two data frames.
#' @param .data A data frame of primary strings.
#' @param lookup A data frame of lookup strings.
#' @return A data frame of matched rows.
#' @importFrom dplyr inner_join
exact_whole_match <- function(.data, lookup) {
  .data |>
    dplyr::inner_join(
      lookup,
      by = "exact_string"
    ) |>
    grab_cols()
}

#' @title Format as Tidytext
#' @description Formats a data frame for tokenized text analysis.
#' @param .data A data frame of strings.
#' @return A tokenized data frame.
#' @importFrom tidytext unnest_tokens
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

#' @title Cross Join
#' @description Performs a cross join between two data frames.
#' @param .data A data frame.
#' @param lookup A lookup data frame.
#' @return A cross-joined data frame.
#' @importFrom dplyr cross_join
cross_join <- function(.data, lookup) {
  dplyr::cross_join(.data, lookup)
}

#' @title Exact Token Match
#' @description Matches tokens exactly between two data frames.
#' @param .data A data frame of tokens.
#' @return A data frame of matched tokens.
exact_token_match <- function(.data) {
  prep_string_token <- lookup_string <- NULL

  .data |>
    dplyr::filter(prep_string_token == prep_string) |>
    grab_cols()
}

#' @title Exact Substring Match
#' @description Matches substrings between two data frames.
#' @param .data A data frame of tokens.
#' @return A data frame of matched substrings.
exact_substring_match <- function(.data) {
  prep_string_token <- lookup_string <- NULL

  .data |>
    dplyr::filter(
      stringr::str_detect(prep_string_token, prep_string)
    ) |>
    grab_cols()
}

#' @title Fuzzy Match
#' @description Performs a fuzzy match between two data frames.
#' @param primary A data frame of primary strings.
#' @param lookup A data frame of lookup strings.
#' @param dist The maximum distance for fuzzy matching.
#' @return A data frame of fuzzy matches.
#' @importFrom stringdist amatch
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

#' @title Tidy Matches
#' @description Performs tidy matches (exact tokens, substrings, and fuzzies).
#' @param primary A data frame of primary strings.
#' @param lookup A data frame of lookup strings.
#' @return A list of tidy matches.
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

#' @title Match
#' @description Matches primary strings against lookup strings using various methods.
#' @param primary A data frame of primary strings.
#' @param lookup A data frame of lookup strings.
#' @return A list of matches.
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
