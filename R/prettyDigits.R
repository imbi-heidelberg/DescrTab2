#' Formatting function for absolute and relative frequencies
#'
#' @param numerator (numeric) numerator for \% calculations
#' @param denominator (numeric) denominator for \% calculations
#' @param absolute_relative_frequency_mode one of \code{c("both", "only_absolute", "only_relative")}.
#' "both" will print "Absolute Freq. (Relative Freq. \%)", the other options work accordingly.
#' @param percent_accuracy NULL or numeric. Refer to the \code{accuracy} argument in
#' \code{\link[scales]{percent}}.
#' @param percent_suffix usually "\%" or "". Refer to the \code{suffix} argument in
#' \code{\link[scales]{percent}}.
#'
#' @return string of formatted frequencies
#' @importFrom scales label_percent
format_freqs <- function(numerator,
                         denominator = 1,
                         absolute_relative_frequency_mode = c(
                           "both",
                           "only_absolute",
                           "only_relative"
                         ),
                         percent_accuracy = NULL,
                         percent_suffix = "%") {
  if (denominator == 0) {
    relfreq <- 0
  } else {
    relfreq <- numerator / denominator
  }

  absolute_relative_frequency_mode <- absolute_relative_frequency_mode[1]
  if (absolute_relative_frequency_mode == "both") {
    paste0(
      numerator,
      " (",
      scales::label_percent(
        accuracy = percent_accuracy,
        suffix = percent_suffix
      )(relfreq),
      ")"
    )
  } else if (absolute_relative_frequency_mode == "only_absolute") {
    as.character(numerator)
  } else if (absolute_relative_frequency_mode == "only_relative") {
    scales::label_percent(
      accuracy = percent_accuracy,
      suffix = percent_suffix
    )(relfreq)
  } else {
    stop(paste(as.character(absolute_relative_frequency_mode), "is not a valid value for absolute_relative_frequency_mode."))
  }
}


#' Digits before decimal -1
#'
#' @details
#' https://stackoverflow.com/questions/47190693/count-the-number-of-integer-digits
#'
#' @param x a
#'
#' @return a
#'
n_int_digits <- function(x) {
  result <- floor(log10(abs(x)))
  result[!is.finite(result)] <- 0
  result
}

#' Format number to a specified number of digits, considering threshold for usage of scientific notation
#'
#' @param x a
#' @param digits a
#' @param scientific_high_threshold a
#' @param scientific_low_threshold a
#' @param force_0_behind_0 a
#'
#' @return a
#'
sigfig <- function(x, digits = 3,
                   scientific_high_threshold = 6,
                   scientific_low_threshold = -6,
                   force_0_behind_0 = FALSE) {
  if (is.na(x)) {
    return("NA")
  } else if (is.numeric(x)) {
    if (n_int_digits(x) + 0.5 > scientific_high_threshold) {
      return(format(x, scientific = TRUE, digits = digits))
    } else if (n_int_digits(x) - 0.5 < scientific_low_threshold) {
      return(format(x, scientific = TRUE, digits = digits))
    }
    if (n_int_digits(x) + 1.5 > digits) {
      if (abs(x) > 2^52) {
        warning("Integers larger than 2^52 might not have an exact floating point represntation.")
      }
      return(format(round(x), scientific = FALSE))
    } else {
      ret <- gsub("\\.$", "", formatC(signif(x, digits = digits), digits = digits, format = "fg", flag = "#"))
      if (isTRUE(force_0_behind_0) & ret == "0") {
        return(format(0, nsmall = digits))
      } else {
        return(ret)
      }
    }
  }
}

#' Generator function for nice formatting functions
#' @inheritParams sigfig
#'
sigfig_gen <- function(digits = 3,
                       scientific_high_threshold = 6,
                       scientific_low_threshold = -6,
                       force_0_behind_0 = FALSE) {
  return(
    function(x) {
      return(sigfig(x,
        scientific_high_threshold = scientific_high_threshold,
        scientific_low_threshold = scientific_low_threshold,
        force_0_behind_0 = force_0_behind_0
      ))
    }
  )
}


good_format <- function(x,
                        force_digits = NULL,
                        soft_digit_suggestion = 4,
                        nsig,
                        scientific_low_threshold = 1e5,
                        scientific_high_threshold = 1e-5,
                        force_nonscientific = FALSE) {
  if (is.na(x)) {
    return("NA")
  } else if (is.numeric(x)) {
    if (isTRUE(force_nonscientific)) {
      if (!is.null(force_digits)) {
        format(round(x, force_digits), nsmall = force_digits)
      } else {

      }
    } else {
      if (!is.null(force_digits)) {
        format(round(x, force_digits), nsmall = force_digits)
      } else if (scientific_high_threshold >= abs(x)) {

      } else if (scientific_low_threshold <= abs(x)) {

      } else if (abs(x) >= 1) {
        formatC(
          round(x, digits = max(0, soft_digit_suggestion - floor(log10(x)))),
          digits = 4,
          format = "fg",
          flag = "#"
        )
      }

      if (abs(x) < .5) {
        formatC(
          signif(x, digits = 4),
          digits = 4,
          format = "fg",
          flag = "#"
        )
      } else if (abs(x) >= 100) {
        formatC(signif(x, digits = 4), digits = 4, format = "fg")
      } else {
        formatC(
          signif(x, digits = 4),
          digits = 4,
          format = "fg",
          flag = "#"
        )
      }
    }
  } else {
    x
  }
}
