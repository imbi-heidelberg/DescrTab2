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
