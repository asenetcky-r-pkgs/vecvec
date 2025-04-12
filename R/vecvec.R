vevec <- function(primary, lookup) {
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

  #standardize
  #index
}
