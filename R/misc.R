#' Function that returns true in CRAN submission
#'
#' @return TRUE for CRAN submission, FALSE otherwise
write_in_tmpfile_for_cran <- function() {
  if (identical(Sys.getenv("NOT_CRAN"), "true")) {
    return(invisible(FALSE))
  } else {
    TRUE
  }
}

explicit_na <- function(f) {
  if (any(is.na(f)))
    fct_na_value_to_level(f, "(Missing)")
  else
    f
}
