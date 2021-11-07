#' Wrap cell text in minipage LaTeX environment with stretchy space
#' @param text text to be placed in minipage
#' @param width width adjustment
#' @param numEscapes (logical) chooses between "\\" and "\\\\"#
#' @param strechSpace (logical) will add strethcy space
#' @return appropriate LaTeX code
#' @references \url{https://stackoverflow.com/a/50892682}
#'
in_minipage <- function(text, width, numEscapes = 1, strechSpace = FALSE) {
  esc <- paste0(rep("\\", numEscapes), collapse = "")
  paste0(
    esc, "begin{minipage}[t]{",
    width, "}",
    esc, "raggedright ",
    if (isTRUE(strechSpace)) {
      paste0(esc, "setstretch{0.5}")
    } else {
      NULL
    },
    text,
    if (isTRUE(strechSpace)) {
      paste0(esc, "vspace{0.75ex}")
    } else {
      NULL
    },
    esc, "end{minipage}"
  )
}

#' Escape LaTeX Symbols
#'
#' @param tibl A \code{tibble} filled with characters
#' @inheritParams in_minipage
#' @return a \code{tibble} with appropriately escape LaTeX code
#'
escape_latex_symbols <- function(tibl, numEscapes = 1) {
  esc <- paste0(rep("\\", numEscapes), collapse = "")
  for (i in 1:nrow(tibl)) {
    for (j in 1:ncol(tibl)) {
      tibl[i, j] <-
        str_replace_all(tibl[i, j], fixed("%"), fixed(paste0(esc, "%")))
      tibl[i, j] <-
        str_replace_all(tibl[i, j], fixed("$"), fixed(paste0(esc, "$")))
      tibl[i, j] <-
        str_replace_all(tibl[i, j], fixed("<"), fixed(paste0(esc, "textless")))
      tibl[i, j] <-
        str_replace_all(tibl[i, j], fixed(">"), fixed(paste0(esc, "textgreater")))
      tibl[i, j] <-
        str_replace_all(tibl[i, j], fixed("_"), fixed(paste0(esc, "_")))
      tibl[i, j] <-
        str_replace_all(tibl[i, j], fixed("&"), fixed(paste0(esc, "&")))
    }
  }
  tibl
}