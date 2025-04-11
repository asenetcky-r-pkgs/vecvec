vecvec <-
  S7::new_class(
    "vecvec",
    properties = list(
      primary = S7::class_character,
      compare = S7::class_character
    ),
    validator = function(self) {
      if (length(self@primary) < 1) {
        "@primary vector must be length > 0"
      } else if (length(self@compare) < 1) {
        "@compare vector must be length > 0"
      }
    }
  )
