pl <- NULL

.onLoad <- function(libname, pkgname) {
  reticulate::py_require("polars")
  pl <<- reticulate::import("polars", delay_load = TRUE)
}
