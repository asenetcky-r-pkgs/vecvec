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
  match(prepped_primary, prepped_lookup)
}
