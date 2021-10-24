#' Extract the label attribute from data
#'
#' @param dat data in the form of a \code{\link[base]{list}}, \code{\link[base]{data.frame}}
#' or \code{\link[tibble]{tibble}}, or a vector
#'
#' @return list of labels
#' @export
#'
#' @examples
#' a <- c(1, 2)
#' attr(a, "label") <- "b"
#' identical(extract_labels(a), list(a = attr(a, "label")))
extract_labels <- function(dat) {
  if (inherits(dat, "list") | inherits(dat, "data.frame") | inherits(dat, "tbl")) {
    extracted_labels <- lapply(dat, function(x) attr(x, "label"))
  } else {
    nm <- deparse(substitute(dat))
    extracted_labels <- list(attr(dat, "label"))
    names(extracted_labels) <- nm
  }
  extracted_labels <- extracted_labels[!sapply(extracted_labels, is.null) &
    !sapply(extracted_labels, function(x) isTRUE(trimws(x) == ""))]
  return(extracted_labels)
}

#' Remove the label attribute from data
#'
#' @inheritParams extract_labels
#'
#' @return data with the labels removed
#' @export
#'
#' @examples
#' a <- c(1, 2)
#' attr(a, "label") <- "b"
#' identical(unlabel(a), c(1, 2))
unlabel <- function(dat) {
  unlabel_fun <- function(x) {
    if (inherits(x, "labelled")) {
      class(x) <- class(x)[class(x) != "labelled"]
      attr(x, "label") <- NULL
    }
    return(x)
  }
  if (inherits(dat, "list") | inherits(dat, "data.frame") | inherits(dat, "tbl")) {
    dat <- lapply(dat, unlabel_fun)
  } else {
    dat <- unlabel_fun(dat)
  }
  dat
}

#' Convencience function to load datasets downloaded from a Redcap database
#'
#' This function is specifically tailored to the way the default import script
#' provided by a Redcap database functions. First, the \code{Hmisc} package is loaded.
#' The .csv file containing the data is assumed to be located in the current working directory.
#' Labels are assigned to all variables. Variables which are supposed to be factors are twice,
#' once as a factor and once in an unformatted way.
#'
#' This script removes the "unformatted factor" variables and properly assignes labels.
#'
#' @param path_to_redcap_script
#'
#' @return tibble with data
#' @export
#' @examples
#' path_to_redcap_script <- system.file("examples", "testredcap.r", package = "DescrTab2")
#' read_redcap_formatted(path_to_redcap_script)
#' @importFrom Hmisc label label<-
read_redcap_formatted <- function(path_to_redcap_script = NULL) {
  stopifnot(is.character(path_to_redcap_script))
  source(path_to_redcap_script, encoding = "UTF-8", local = TRUE)
  data <- as_tibble(data)
  colnames_data <- names(data)
  for (colname in colnames_data) {
    if (str_detect(colname, "\\.factor$")) {
      fac <- data[[colname]]
      label(fac) <- label(data[[str_remove(colname, "\\.factor$")]])
      data[str_remove(colname, "\\.factor$")] <- fac
      data <- data[names(data) != colname]
    }
  }
  data
}

#' Split a dataset imported from Redcap into convenient subsets
#'
#' This function seperates a datasets into three parts: "Singular" data, which is the
#' data from non-repeating instruments. "missings_everywhere", which is data which is missing for each row.
#' The last parts are all the repeating instruments, which are referred to by their name as recorded in
#' \code{dat$redcap_repeat_instrument}.
#'
#' @param dat a \code{tibble} produced by \code{\link{read_redcap_formatted}}.
#' @param id_name (character) the name of the subject ID variable.
#'
#' @return a list of datasets separated into the categories as described
#' @export
#'
#' @examples
#' path_to_redcap_script <- system.file("examples", "testredcap.r", package = "DescrTab2")
#' dat <- read_redcap_formatted(path_to_redcap_script)
#' d <- split_redcap_dataset(dat, guess_ID_variable(dat, TRUE))
split_redcap_dataset <- function(dat, id_name = "patid") {
  missings_everywhere <-
    dat %>% select(!!id_name, where(~ (all(is.na(.x)) | all(.x == ""))))

  d <-
    lapply(
      as.character(unique(
        fct_explicit_na(dat$redcap_repeat_instrument)
      )),
      function(x) {
        dat %>%
          filter(fct_explicit_na(redcap_repeat_instrument) == !!x)
      }
    )

  names(d) <-
    as.character(unique(fct_explicit_na(dat$redcap_repeat_instrument)))
  names(d)[names(d) == "(Missing)"] <- "Singular"

  d <- lapply(d, function(x) {
    x %>% select(where(~ !(all(is.na(.x)) | all(.x == ""))))
  })

  d[["missings_everywhere"]] <- missings_everywhere
  d
}

#' Convencience function to load SAS datasets
#'
#' @param path_to_data path to .sas7bdat file
#' @param path_to_format path to .sas7bcat file
#'
#' @return tibble with data
#' @export
#'
#' @examples
#' path_to_data <- system.file("examples", "testsas.sas7bdat", package = "DescrTab2")
#' pat_to_format <- system.file("examples", "formats.sas7bcat", package = "DescrTab2")
#' haven::read_sas(path_to_data, pat_to_format)
#' @importFrom haven read_sas
#'
read_sas_formatted <- function(path_to_data = NULL, path_to_format = NULL) {
  erg <- read_sas(
    path_to_data,
    path_to_format
  )
  erg <- erg %>%
    mutate(across(where(function(x) inherits(x, "haven_labelled")), as_factor))
  erg
}



#' Create a markdown listing from a character dataset
#'
#' @param dat a character \code{data.frame} or \code{tibble}.
#'
#' @return string containing markdown code listing all nonempty free text in the dataset
#' @export
#'
list_freetext_markdown <- function(dat) {
  dat <- as_tibble(dat)
  str <- ""
  for (i in 1:ncol(dat)) {
    var <- pull(dat, !!i)
    name <- names(dat[, i])[1]
    lab <- attr(var, "label")
    print_name <- if (is.null(lab)) name else paste0(lab, " (", name, ")")
    var <- var[!(var %in% c("", NA_character_))]
    if (length(var) > 0) {
      namerow <- paste0("**", print_name, "**\n\n")
      varrows <- paste0(" * ", var, "\n\n")
      str <- paste0(str, namerow, paste0(varrows, collapse = ""), collapse = "")
    }
  }
  str
}



#' Parse a text file containing format information
#'
#' Useful to extract factor formatting information contained in a proc format SAS statement.
#'
#' @param path_to_format_definition (string) Path to the text file to be parsed
#' @param ignore_keywords A vector of keywords to be ignored when searching for the name of
#' the variable to be formatted
#' @param encoding Encoding for the text file
#'
#' @return
#' @export
#'
#' @examples
parse_formats <- function(path_to_format_definition,
                          ignore_keywords = c("value"),
                          encoding = "ISO-8859-1") {
  ff <- file(path_to_format_definition, encoding = encoding)
  f <- readLines(ff) %>%
    paste0(collapse = "")
  close(ff)

  # strip "proc format;" and "run;" from the file
  tmp <- str_extract(f, "(?<=([pP][rR][oO][cC] [fF][oO][rR][mM][aA][tT])).*(?=[rR][uU][nN];)")
  # strip all comments delminited by /*  */
  tmp <- str_remove_all(tmp, "\\/\\*.*?\\*\\/")

  tmp <- strsplit(tmp, "")[[1]]

  i <- 1L

  strbuf <- ""
  current_delimiter <- NULL
  start_delim_index <- NULL
  end_delim_index <- NULL
  levels <- numeric()
  labels <- character()
  parse_varname <- TRUE
  parse_level <- FALSE
  parse_label <- FALSE

  format_list <- list()
  while (i <= length(tmp)) {
    current_char <- tmp[i]

    if (parse_varname) {
      strbuf <- paste0(strbuf, current_char)
      current_word <- str_extract(strbuf, "(?<=[\\s\\;])[^\\s]+(?=\\s)")
      if (!is.na(current_word)) {
        if (current_word %in% ignore_keywords) {
          strbuf <- str_replace(strbuf, "(?<=[\\s\\;])[^\\s]+(?=\\s)", "")
        } else {
          labels <- character()
          if (isTRUE(str_split(current_word, "")[[1]][[1]] == "$")) {
            char_factor <- TRUE
            varname <- paste0(str_split(current_word, "")[[1]][-1], collapse = "")
            levels <- character()
          } else {
            char_factor <- FALSE
            varname <- current_word
            levels <- numeric()
          }
          parse_varname <- FALSE
          parse_level <- TRUE
          strbuf <- current_char
        }
      }
    }
    if (parse_level) {
      if (isTRUE(current_char == ";")) {
        parse_level <- FALSE
        parse_varname <- TRUE
        current_delimiter <- NULL
        format_list[[varname]] <- list(levels = levels, labels = labels)
      }
      if (char_factor) {
        if (isTRUE(current_char == current_delimiter) | (isTRUE(str_detect(current_delimiter, "\\s")) && isTRUE(current_char == "="))) {
          end_delim_index <- i
        }
        if (!is.null(end_delim_index)) {
          current_level <- paste0(tmp[(start_delim_index + 1):(end_delim_index - 1)], collapse = "")
          if (!is.na(current_level)) {
            levels[length(levels) + 1] <- current_level
            parse_level <- FALSE
            parse_label <- TRUE
            current_delimiter <- NULL
            start_delim_index <- NULL
            end_delim_index <- NULL
            strbuf <- current_char
            i <- i + 1L
            next
          }
        }
        if (is.null(current_delimiter) && isTRUE(str_detect(current_char, "[\\'\"[:alpha:]\\.]"))) {
          if (str_detect(current_char, "[[:alpha:]\\.]")) {
            start_delim_index <- i - 1
            current_delimiter <- tmp[i - 1]
          } else {
            start_delim_index <- i
            current_delimiter <- current_char
          }
        }
      } else {
        strbuf <- paste0(strbuf, current_char)
        current_level <- str_extract(strbuf, "(?<=\\s)[[:digit:]\\.\\-]+(?=[\\s\\=])")
        if (!is.na(current_level)) {
          current_level <- if (isTRUE(current_level == ".")) NA_real_ else as.numeric(current_level)
          levels[length(levels) + 1] <- current_level
          parse_label <- TRUE
          parse_level <- FALSE
          strbuf <- current_char
        }
      }
    }
    if (parse_label) {
      if (isTRUE(current_char == current_delimiter) | (isTRUE(str_detect(current_delimiter, "\\s")) && isTRUE(current_char == ";"))) {
        end_delim_index <- i
      }
      if (!is.null(end_delim_index)) {
        current_label <- paste0(tmp[(start_delim_index + 1):(end_delim_index - 1)], collapse = "")
        if (!is.na(current_label)) {
          labels[length(labels) + 1] <- current_label
          parse_label <- FALSE
          if (isTRUE(current_char == ";")) {
            format_list[[varname]] <- list(levels = levels, labels = labels)
            parse_varname <- TRUE
          } else {
            parse_level <- TRUE
          }
          current_delimiter <- NULL
          start_delim_index <- NULL
          end_delim_index <- NULL
          strbuf <- current_char
          i <- i + 1L
          next
        }
      }
      if (is.null(current_delimiter) && str_detect(current_char, "[\\'\"[:alpha:]\\.]")) {
        if (str_detect(current_char, "[[:alpha:]\\.]")) {
          start_delim_index <- i - 1
          current_delimiter <- tmp[i - 1]
        } else {
          start_delim_index <- i
          current_delimiter <- current_char
        }
      }
    }
    i <- i + 1L
  }
  return(format_list)
}


#' Create code to load all SAS datasets in a folder.
#'
#' This is useful if you work with lots of separate SAS datasets spread out in the same folder.
#'
#' @param dir path to dataset folder
#' @param format path to format file
#'
#' @return NULL. Relevant code is printed to the console.
#' @export
#'
codegen_load_all_sas_data <- function(dir, format = NULL) {
  e <- str_subset(list.files(dir), "\\.sas7bdat$")
  p <- paste0(dir, e)
  cat(paste0(
    "d.", str_replace(e, "\\.sas7bdat$", ""), " <- haven::read_sas(data_file = \"", p,
    "\", catalog_file = \"", format, "\")\n"
  ))
  cat(paste0("d <- list(", paste0("d.", str_replace(e, "\\.sas7bdat$", ""), collapse = ", "), ")\n"))
  cat(paste0("names(d) <- ", paste0("c(", paste0(paste0("\"", str_replace(e, "\\.sas7bdat$", ""), "\""),
    collapse = ", "
  ), ")")))
  return(NULL)
}

#' Make an educated guess about the name of the ID variable from a dataset
#'
#' @param dat a dataset with names (\code{list}, \code{data.frame}, \code{tibble})
#' @param suppressWarnings (logical) suppress warning messages if you know what you are dooing
#'
#' @return if exactly one possible
#' @export
#'
#' @examples
#' @importFrom stringr str_to_lower
#' @importFrom magrittr `%>%`
guess_ID_variable <- function(dat, suppressWarnings = FALSE) {
  original_colnames <- names(dat)
  colnames <- str_to_lower(original_colnames)
  # common words to describe subject id
  ids1 <- str_detect(
    colnames,
    "(^subjectid$)|(^subid$)|(^subject$)|
(^patientid$)|(^patid$)|(^patient$)|
(^screeningno$)|(^screeno$)|(^screenno$)(^scrno$)|
(^randomid$)|(^randid$)|(^ranid$)|(^randomno$)|(^randno$)|(^ranno$)|(^rano$)
(^indices$)|(^index$)|(^idx$)"
  )
  # regex to catch "id" if it is not part of another word
  ids2 <- str_detect(colnames, "(?<![:alpha:])id(?![:alpha:])")
  ids <- original_colnames[ids1 | ids2]
  if (length(ids) > 1) {
    if (!isTRUE(suppressWarnings)) {
      warning("Multiple possible ID variables found. No candidate is chosen automatically.")
    }
    return(NULL)
  } else if (length(ids) == 1) {
    if (!isTRUE(suppressWarnings)) {
      warning(paste0(
        "One possible candidate for an ID variable found: ", ids,
        ". This variable is used as ID."
      ))
    }
    return(ids)
  } else {
    return(NULL)
  }
}

# work in progress
split_data_from_listing <- function(dat, n_split_levels = 10, n_split_characters = 35) {
  dat <- as_tibble(dat)
  idx_dat <- list()
  idx_list <- list()
  for (i in 1:ncol(dat)) {
    too_many_levels <- (is.character(pull(dat, !!i)) | inherits(pull(dat, !!i), "Date")) &
      (length(levels(factor(pull(dat, !!i)))) > n_split_levels)
    label_too_long <- length(attr(pull(dat, !!i), "label")) > n_split_characters
    name_too_long <- if (is.null(attr(pull(dat, !!i), "label"))) FALSE else length(names(dat[, i])[1]) > n_split_characters
    factor_levels_too_long <- (is.character(pull(dat, !!i)) | inherits(pull(dat, !!i), "Date") | is.factor(pull(dat, !!i))) &
      any(sapply(levels(factor(pull(dat, !!i))), function(x) length(x) > n_split_characters))

    if (too_many_levels | label_too_long | name_too_long | factor_levels_too_long) {
      idx_list[[length(idx_list) + 1]] <- i
    } else {
      idx_dat[[length(idx_dat) + 1]] <- i
    }
  }
  return(list(
    data = dat[, unlist(idx_dat)],
    list = dat[, unlist(idx_list)]
  ))
}
