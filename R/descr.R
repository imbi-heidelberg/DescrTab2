#' Create a descriptive statistics table
#'
#' Generate a table of descriptive statistics with p-values obtained in tests
#' for difference between the groups.
#'
#' @param dat
#' Data frame or tibble. The data set to be analyzed. Can contain continuous or factor (also ordered) variables.
#' @param group
#' @param var_options
#' @param group_labels
#' @param summary_stats_cont
#' @param summary_stats_cat
#' @param format_p
#' @param format_summary_stats
#' @param format_options
#' @param test_options
#' @param ...
#' Vector of the grouping variable.
#' @return
#' Depending on the value of the create parameter either pdf, word, tex, R
#' or an file optimized for use in connection with knitr will be created containing
#' the descriptive statistics table with the speak for the document to create in the following.
#' For example you choose \code{create="pdf"} then the table is written in \code{TeX}-Code.
#' Attention: the table has no caption and numbers of observations per group.
#'
#' @author Jan Meis, Lorenz Uhlmann, Csilla van Lunteren
#'
#' @examples
#' \dontrun{
#' }
#'
#' @export
#'
#' @import dplyr
#' @import magrittr
#' @import tibble
#' @import forcats
#' @import stringr
#' @import flextable
#' @import kableExtra
#'
descr <-
  function(dat,
           group = NULL,
           var_options = list(),
           group_labels = list(),

           summary_stats_cont = list(
             N = .N,
             Nmiss = .Nmiss,
             mean = .mean,
             sd = .sd,
             median = .median,
             Q1 = .Q1,
             Q3 = .Q3,
             min = .min,
             max = .max
           ),

           summary_stats_cat = list(),
           format_p = scales::pvalue_format(),
           format_summary_stats = list(
             N = function(x)
               format(x, digits = 2, scientific = 3),
             Nmiss = function(x)
               format(x, digits = 2, scientific = 3),
             mean = function(x)
               format(x, digits = 2, scientific = 3),
             sd = function(x)
               format(x, digits = 2, scientific = 3),
             median = function(x)
               format(x, digits = 2, scientific = 3),
             Q = function(x)
               format(x, digits = 2, scientific = 3),
             minmax = function(x)
               format(x, digits = 2, scientific = 3),
             CI = function(x)
               format(x, digits = 2, scientific = 3)
           ),
           format_options = list(
             print_p = T,
             print_CI = T,
             omit_Nmiss_if_0 = T,
             omit_missings_in_group = F,
             make_missing_a_category = F
           ),
           test_options = list(
             paired = F,
             nonparametric = F,
             exact = F,
             indices = c(),
             include_group_missings_in_test = F,
             include_categorical_missings_in_test = F
           ),
           ...) {
    # Coerce dataset to tibble
    dat %<>% as_tibble(dat)

    # If options lists were passed as named named vectors, coerce to list
    var_options = lapply(var_options, as.list)
    group_labels = lapply(group_labels, as.list)

    # Remove group column from dataset & coerce group to factor
    if (!is.null(group)) {
      group_var <-
        dat %>% pull(all_of(group)) %>% as_factor() %>% fct_explicit_na()
      dat %<>% select(-all_of(group))
    } else{
      group_var <- NULL
    }

    # Coerce all non-numeric columns to factors
    dat %<>% mutate(across(-where(is.numeric), function(x)
      x %>% as_factor() %>% fct_explicit_na()))

    # Create list where all results will be saved
    ergs <- list()
    ergs[["variables"]] <- list()
    ergs[["group"]][["var"]] <- group_var
    ergs[["group"]][["name"]] <- group

    # Loop over all variables
    for (var_name in names(dat)) {
      var <- dat %>% pull(var_name)

      var_descr <- NULL
      if (is.factor(var)) {
        # Analyze categorical variable
        var_descr <- descr_cat(var,
                               group_var,
                               var_name,
                               summary_stats_cat,
                               var_options = var_options[[var_name]],
                               test_options)
      } else if (is.numeric(var)) {
        # Analyze continuous variable
        var_descr <- descr_cont(var,
                                group_var,
                                var_name,
                                summary_stats_cont,
                                var_options = var_options[[var_name]],
                                test_options)
      } else{
        stop("Somehow, you have variables which are neither factors nor numerical.")
      }
      # Append result of analysis to list
      ergs[["variables"]][[var_name]] <- var_descr
    }

    # Save formatting options for printing later
    ergs[["var_options"]] <- var_options
    ergs[["group_labels"]] <- group_labels
    ergs[["format"]][["p"]] <- format_p
    ergs[["format"]][["summary_stats"]] <- format_summary_stats
    ergs[["format"]][["options"]] <- format_options
    ergs[["input_facts"]] <- list(nrow = nrow(dat), ncol= ncol(dat))

    # Make result a "DescrList" object and return
    attr(ergs, "class") <- c("DescrList", "list")
    return(ergs)
  }


#' Create descriptive statistics for a categorical variable
#'
#' @param var
#' @param group
#'
#' @return
#' @export
#'
#' @examples
#'
#' @import dplyr
#' @import magrittr
#' @import tibble
#' @import forcats
#' @import stringr
#' @import flextable
#' @import kableExtra
descr_cat <-
  function(var,
           group,
           var_name,
           summary_stats = c(),
           var_options = list(),
           test_options = list()) {
    erg <- list()
    var_levels <- levels(var)

    # Summary stats choice: Special variable summary stats have precendence over global summary stats
    if (!is.null(var_options[["summary_stats"]])) {
      summary_stats <- var_options[["summary_stats"]]
    }


    for (group_name in levels(group)) {
      # Subset values for the respective group
      var_grp <- var[which(group == group_name)]
      cat_list <- list()

      for (summary_stat_name in names(summary_stats)) {
        cat_list[[summary_stat_name]] <-
          summary_stats[[summary_stat_name]](var_grp)
      }

      for (cat_name in var_levels) {
        cat_list[[cat_name]] <- sum(var_grp == cat_name)
      }
      erg[[group_name]] <- cat_list
    }

    # Caclulate summary for whole cohort
    cat_list <- list()
    for (summary_stat_name in names(summary_stats)) {
      cat_list[[summary_stat_name]] <-
        summary_stats[[summary_stat_name]](var)
    }
    for (cat_name in var_levels) {
      cat_list[[cat_name]] <- sum(var == cat_name)
    }
    erg[["Total"]] <- cat_list

    # Check if a specific test is requested for this variable
    test <- var_options[["test"]]
    # Calculate test
    erg[["test_list"]] <- test_cat(var, group, test_options, test)
    erg[["variable_name"]] <- var_name
    erg[["variable_levels"]] <- var_levels
    erg[["variable_options"]] <- var_options

    attr(erg, "class") <- c("cat_summary", "list")
    erg
  }


#' Create descriptive statistics for a continuous variable
#'
#' @param var
#' @param group
#'
#' @return
#' @export
#'
#' @examples
#'
#' @import dplyr
#' @import magrittr
#' @import tibble
#' @import forcats
#' @import stringr
#' @import flextable
#' @import kableExtra
descr_cont <-
  function(var,
           group,
           var_name,
           summary_stats = c(),
           var_options = list(),
           test_options = list()) {
    erg <- list()

    # Summary stats choice: Special variable summary stats have precendence over global summary stats which in turn have precedence over
    # the default summary stats (which are N, Nmiss, mean, sd, median, Q1, Q3, min and max).
    # Summary stats choice: Special variable summary stats have precendence over global summary stats
    if (!is.null(var_options[["summary_stats"]])) {
      summary_stats <- var_options[["summary_stats"]]
    }

    for (group_name in levels(group)) {
      # Subset values for the respective group
      var_grp <- var[which(group == group_name)]
      group_list <- list()

      for (summary_stat_name in names(summary_stats)) {
        group_list[[summary_stat_name]] <-
          summary_stats[[summary_stat_name]](var_grp)
      }
      erg[[group_name]] <- group_list
    }

    # Calculate summary for whole cohort
    tot_list <- list()

    for (summary_stat_name in names(summary_stats)) {
      tot_list[[summary_stat_name]] <-
        summary_stats[[summary_stat_name]](var)
    }
    erg[["Total"]] <- tot_list

    # Check if a specific test is requested for this variable
    test <- var_options[["test"]]
    # Calculate test
    erg[["test_list"]] <- test_cont(var, group, test_options, test)
    erg[["variable_name"]] <- var_name
    erg[["variable_options"]] <- var_options

    attr(erg, "class") <- c("cont_summary", "list")
    erg
  }


#' S3 override for print function for DescrList objects
#'
#' @param DescrListObj
#' @param printFormat
#' Possible values: "console" (default), "tex", "html", "word", "numeric"
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
#' @import dplyr
#' @import magrittr
#' @import tibble
#' @import forcats
#' @import stringr
#' @import flextable
#' @import kableExtra
print.DescrList <-  function(DescrListObj,
                             printFormat = options("DescrTabFormat")[[1]],

                             var_options = list(),
                             group_labels = list(),

                             format_p = NULL,
                             format_summary_stats = list(),
                             format_options = list(),
                             ...) {
  # Overwrite formatting options if they were resupplied in the print step function call
  if (length(var_options) > 0) {
    DescrListObj[["var_options"]] <- var_options
  }
  if (length(group_labels) > 0) {
    DescrListObj[["group_labels"]] <- group_labels
  }
  if (!is.null(format_p)) {
    DescrListObj[["format"]][["p"]] <- format_p
  }
  if (length(format_summary_stats) > 0) {
    DescrListObj[["format"]][["summary_stats"]] <- format_summary_stats
  }
  if (length(format_options) > 0) {
    DescrListObj[["format"]][["options"]] <- format_options
  }

  # if no printing format was set, print to console
  if (is.null(printFormat)) {
    printFormat <- "console"
  }

  # Preprocessing of the DescrListObj for printing.
  # In this step, formatting rules are applied.
  DescrPrintObj <- create_printObj(DescrListObj, printFormat)

  # Depending on the selected printing format, appropriate post-processing of the PrintObject is performed and the result is printed.
  # For "tex" & "html" output is created by kableExtra and the functions are nearly identical. The difference is that in tex, some characters have to
  # escaped to be rendered properly (i.e. \\ has to be insereted before).
  # "word" tables are printed using flextable.
  # "numeric" and "console" tables are printed similarily as tibbles would be printed to the console.
  ret <- switch(
    printFormat,
    tex = print_tex(DescrPrintObj),
    html = print_html(DescrPrintObj),
    word = print_word(DescrPrintObj),
    numeric = print_numeric(DescrPrintObj, ...),
    print_console(DescrPrintObj, ...)
  )

  invisible(ret)
}

create_printObj <- function(DescrListObj, printFormat) {
  create_subtable <- switch (printFormat,
                             numeric = create_numeric_subtable,
                             create_character_subtable)

  var_names <- names(DescrListObj[["variables"]])
  group_names <- c(DescrListObj[["group"]][["var"]] %>% levels(),
                   "Total")


  format_summary_stats <-
    DescrListObj[["format"]][["summary_stats"]]
  format_p <- DescrListObj[["format"]][["p"]]
  format_options <- DescrListObj[["format"]][["options"]]

  print_list <- list()

  for (var_name in var_names) {
    print_list[[var_name]] <-
      # TODO: remove var_name dependency
      DescrListObj[["variables"]][[var_name]] %>% create_subtable(.,
                                                                  var_name,
                                                                  format_options,
                                                                  format_summary_stats,
                                                                  format_p)
  }

  printObj <- list()
  printObj[["variables"]] <- list()
  printObj[["lengths"]] <- list()
  printObj[["group"]] <- DescrListObj[["group"]]

  group_n <- c()
  for (lvl in levels(DescrListObj[["group"]][["var"]])) {
    group_n <- c(group_n, sum(DescrListObj[["group"]][["var"]] == lvl))
  }
  ## Reminder: Add option to exclude Missings
  if (is.null(DescrListObj[["group"]])){
    group_n <- DescrListObj[["input_facts"]][["nrow"]]
  } else{
    group_n <- c(group_n, sum(group_n))
  }


  printObj[["group_n"]] <- group_n
  printObj[["group_names"]] <- group_names

  group_labels <- c()
  for (name in group_names) {
    if (!is.null(DescrListObj[["group_labels"]][[name]])) {
      group_labels <-
        c(group_labels, DescrListObj[["group_labels"]][[name]])
    } else{
      group_labels <- c(group_labels, name)
    }
  }
  printObj[["group_labels"]] <- group_labels

  tibl <- tibble()

  for (var_name in var_names) {
    printObj[["variables"]][[var_name]] <-
      print_list[[var_name]][["summary_list"]]
    printObj[["lengths"]][[var_name]] <-
      print_list[[var_name]][["length"]]
    printObj[["labels"]][[var_name]] <-
      print_list[[var_name]][["summary_list"]][["label"]]

    tibl %<>%  bind_rows(print_list[[var_name]][["tibble"]])
  }

  if (isTRUE(DescrListObj[["format"]][["options"]][["print_p"]] == F)) {
    tibl %<>% select(-p)
    tibl %<>% select(-Test)
  }

  if (isTRUE(DescrListObj[["format"]][["options"]][["print_CI"]] == F)) {
    tibl %<>% select(-CI)
  }

  names(tibl)[names(tibl) %in% group_names] <- group_labels
  printObj[["tibble"]] <- tibl
  attr(printObj, "class") <- c("printObj", "list")
  printObj
}


#' Title
#'
#' @param DescrListObj
#' @param n
#' @param width
#' @param n_extra
#' @param print_red_NA
#'
#' @return
#' @export
#' @examples
#'
#' @import dplyr
#' @import magrittr
#' @import tibble
#' @import forcats
#' @import stringr
#' @import flextable
#' @import kableExtra
print_numeric <- function(DescrPrintObj,
                          n = 1000,
                          width = NULL,
                          n_extra = NULL,
                          print_red_NA = F) {
  tibl <- DescrPrintObj[["tibble"]]

  labels <- unlist(unlist(DescrPrintObj[["labels"]]))
  c1 <- tibl %>% pull(1)
  c1 <- ifelse(c1 %in% labels, c1, paste0("  ", c1))
  tibl[, 1] <- c1

  print_format <- format(tibl,
                         n = n,
                         width = width,
                         n_extra = n_extra) %>%  str_replace_all(pattern = fixed('"'), fixed(' '))

  if (print_red_NA) {
    print_format %>% cli::cat_line()
  } else{
    print_format %>% str_replace_all(pattern = fixed("\033[31mNA\033[39m"),
                                     fixed("\033[31m  \033[39m")) %>%  cli::cat_line()
  }
  invisible(DescrPrintObj)
}


#' Title
#'
#' @param DescrListObj
#' @param n
#' @param width
#' @param n_extra
#' @param print_red_NA
#'
#' @return
#' @export
#'
#' @examples
#'
#' @import dplyr
#' @import magrittr
#' @import tibble
#' @import forcats
#' @import stringr
#' @import flextable
#' @import kableExtra
print_console <- function(DescrPrintObj,
                          n = 1000,
                          width = NULL,
                          n_extra = NULL,
                          print_red_NA = F) {
  tibl <- DescrPrintObj[["tibble"]]

  labels <- unlist(unlist(DescrPrintObj[["labels"]]))
  c1 <- tibl %>% pull(1)
  c1 <- ifelse(c1 %in% labels, c1, paste0("  ", c1))
  tibl[, 1] <- c1

  print_format <- format(tibl,
                         n = n,
                         width = width,
                         n_extra = n_extra)

  print_format %>% .[-c(1, 3)] %>%
    str_replace_all(pattern = fixed('"'), fixed(' ')) %>%
    cli::cat_line()

  invisible(DescrPrintObj)
}

#' Title
#'
#' @param DescrListObj
#' @param n
#' @param width
#' @param n_extra
#' @param print_red_NA
#'
#' @return
#' @export
#'
#' @examples
#'
#' @import dplyr
#' @import magrittr
#' @import tibble
#' @import forcats
#' @import stringr
#' @import flextable
#' @import kableExtra
print_tex <- function(DescrPrintObj) {
  tibl <- DescrPrintObj[["tibble"]]
  var_names <- names(DescrPrintObj[["variables"]])
  lengths <- c(unlist(DescrPrintObj[["lengths"]]) - 1)

  labels <- unlist(unlist(DescrPrintObj[["labels"]]))

  names(lengths) <- c(labels)

  c1 <- tibl %>% pull(1)
  indx_varnames <- c1 %in% labels


  if ("p" %in% names(tibl)) {
    print_footnotes <- T
    tests <- tibl %>% filter(Test != "") %>% pull(Test) %>% unique()
    p_vec <- tibl %>% pull(p)
    p_indx <- which(p_vec != "")
    test_abbrev <- create_test_abbreviations(tests)
    for (idx in p_indx) {
      tibl[idx, "p"] %<>% paste0("\\textsuperscript{", test_abbrev[match(tibl[idx, "Test"], tests)]  , "}")
    }
  } else{
    print_footnotes <- F
  }


  tibl %<>% select(-Test)
  alig <- paste0(c("l", rep("c", ncol(tibl) - 1)), collapse = "")
  alig2 <- paste0(c("l", rep("c", ncol(tibl) - 1)))
  actual_colnames <- names(tibl[!indx_varnames,])

  N_numbers <-
    c("", paste0("(N=", DescrPrintObj[["group_n"]], ")"))
  pad_N <- ncol(tibl) - length(N_numbers)
  N_numbers <- c(N_numbers, rep("", pad_N))

  tibl <- escape_latex_symbols(tibl)


  tex <- tibl[!indx_varnames,] %>%
    kbl(
      format = "latex",
      longtable = T,
      booktabs = T,
      linesep = "",
      align = alig,
      escape = F,
      col.names = N_numbers
    ) %>%
    kable_styling() %>%
    `if`(
      print_footnotes,
      kableExtra::footnote(., symbol = c(tests), symbol_manual = test_abbrev),
      .
    ) %>%
    pack_rows(index = lengths) %>%
    add_header_above(actual_colnames, line = F, align = alig2) %>%
    capture.output()

  tex %<>% str_replace_all(fixed("\\\\"), fixed("\\\\*"))
  pagebreak_indices <-
    str_detect(tex, fixed("textbf")) %>% which() %>% tail(-1)
  if (length(head(pagebreak_indices,-1)) > 0) {
    tex[head(pagebreak_indices,-1) - 2] %<>% str_replace_all(fixed("\\\\*"),
                                                             fixed("\\\\ \\noalign{\\vskip 0pt plus 12pt}"))
  }
  if (length(tail(pagebreak_indices, 1))) {
    tex[tail(pagebreak_indices, 1) - 2] %<>% str_replace_all(
      fixed("\\\\*"),
      fixed(
        "\\\\ \\noalign{\\vskip 0pt plus 12pt} \\noalign{\\penalty-5000}"
      )
    )
  }
  tex <- c("\\needspace{2cm}", tex)
  cli::cat_line(tex)
  invisible(DescrPrintObj)
}


#' Title
#'
#' @param DescrListObj
#' @param n
#' @param width
#' @param n_extra
#' @param print_red_NA
#'
#' @return
#' @export
#'
#' @examples
#'
#' @import dplyr
#' @import magrittr
#' @import tibble
#' @import forcats
#' @import stringr
#' @import flextable
#' @import kableExtra
print_html <- function(DescrPrintObj) {
  tibl <- DescrPrintObj[["tibble"]]
  var_names <- names(DescrPrintObj[["variables"]])
  lengths <- c(unlist(DescrPrintObj[["lengths"]]) - 1)


  labels <- unlist(unlist(DescrPrintObj[["labels"]]))
  names(lengths) <- c(labels)

  c1 <- tibl %>% pull(1)
  indx_varnames <- c1 %in% labels

  if ("p" %in% names(tibl)) {
    print_footnotes <- T
    tests <- tibl %>% filter(Test != "") %>% pull(Test) %>% unique()
    p_vec <- tibl %>% pull(p)
    p_indx <- which(p_vec != "")
    test_abbrev <- create_test_abbreviations(tests)
    for (idx in p_indx) {
      tibl[idx, "p"] %<>% paste0("<sup>", test_abbrev[match(tibl[idx, "Test"], tests)]  , "</sup>")
    }
  } else{
    print_footnotes <- F
  }


  if ("Test" %in% names(tibl)) {
    tibl %<>% select(-Test)
  }



  alig <- paste0(c("l", rep("c", ncol(tibl) - 1)), collapse = "")
  alig2 <- paste0(c("l", rep("c", ncol(tibl) - 1)))
  actual_colnames <- names(tibl[!indx_varnames,])
  N_numbers <-
    c("", paste0("(N=", DescrPrintObj[["group_n"]], ")"))
  pad_N <- ncol(tibl) - length(N_numbers)
  N_numbers <- c(N_numbers, rep("", pad_N))


  tibl[!indx_varnames,] %>%
    kbl(
      format = "html",
      longtable = T,
      booktabs = T,
      linesep = "",
      align = alig,
      escape = F,
      col.names = N_numbers
    ) %>%
    kable_styling() %>%
    `if`(
      print_footnotes,
      kableExtra::footnote(., symbol = c(tests), symbol_manual = test_abbrev),
      .
    ) %>%
    pack_rows(index = lengths) %>%
    add_header_above(actual_colnames, line = F, align = alig2) %>%
    cat() %>%
    knitr::raw_html()

  invisible(DescrPrintObj)
}


#' Title
#'
#' @param DescrListObj
#' @param n
#' @param width
#' @param n_extra
#' @param print_red_NA
#'
#' @return
#' @export
#'
#' @examples
#'
#' @import dplyr
#' @import magrittr
#' @import tibble
#' @import forcats
#' @import stringr
#' @import flextable
#' @import kableExtra
print_word <- function(DescrPrintObj) {
  tibl <- DescrPrintObj[["tibble"]]
  var_names <- names(DescrPrintObj[["variables"]])
  lengths <- c(unlist(DescrPrintObj[["lengths"]]) - 1)
  names(lengths) <- c(var_names)

  c1 <- tibl %>% pull(1)
  indx_varnames <- c1 %in% var_names

  if ("p" %in% names(tibl)) {
    print_footnotes <- T
    tests <- tibl %>% filter(Test != "") %>% pull(Test) %>% unique()
    p_vec <- tibl %>% pull(p)
    p_indx <- which(p_vec != "")
    test_abbrev <- create_test_abbreviations(tests)
  } else{
    print_footnotes <- F
  }

  if ("Test" %in% names(tibl)) {
    tibl2 <- tibl %>%  select(-Test)
  } else{
    tibl2 <- tibl
  }


  actual_colnames <- DescrPrintObj[["group_names"]]
  N_numbers <- c(paste0("(N=", DescrPrintObj[["group_n"]], ")"))
  names(N_numbers) <- actual_colnames


  ft <- tibl2 %>%
    flextable() %>%
    bold(i = indx_varnames, j = 1) %>%
    padding(j = 1,
            i = !indx_varnames,
            padding.left = 20) %>%
    add_header(top = F, values = N_numbers, ) %>%
    border_inner(part = "header", border = officer::fp_border(width = 0)) %>%
    hline_bottom(part = "header", border = officer::fp_border(width = 2)) %>%
    align(j = which(names(tibl2) != "Variables"),
          part = "all",
          align = "center") %>%
    align(j = 1, part = "all",
          align = "left")

  if (print_footnotes) {
    for (test in tests) {
      ft %<>%  flextable::footnote(
        i =  which((tibl %>% pull(Test)) %in% test),
        j =  which(names(tibl2) == "p"),
        value = as_paragraph(c(test)),
        ref_symbols = c(test_abbrev[match(test, tests)]),
        part = "body"
      )
    }
  }

  ft <- ft %>%
    autofit()

  DescrPrintObj[["ft"]] <- ft
  return(DescrPrintObj)
}


#' S3 dispatcher for subtable creation
#'
#' @param DescrVarObj
#' @param var_name
#'
#' @return
#' @export
#'
#' @examples
#'
#' @import dplyr
#' @import magrittr
#' @import tibble
#' @import forcats
#' @import stringr
#' @import flextable
#' @import kableExtra
create_numeric_subtable <- function(DescrVarObj, ...) {
  UseMethod("create_numeric_subtable")
}


#' Create subtables for categorical variables which will comprise the output table
#'
#' @param DescrVarObj
#' @param var_name
#'
#' @return
#' @export
#'
#' @examples
#'
#' @import dplyr
#' @import magrittr
#' @import tibble
#' @import forcats
#' @import stringr
#' @import flextable
#' @import kableExtra
create_numeric_subtable.cat_summary <-
  function(DescrVarObj,
           var_name,
           format_options,
           format_summary_stats,
           format_p) {
    ## Remember: Category levels may not be names "N"
    cat_names <- DescrVarObj[["variable_levels"]]
    summary_stat_names <-
      setdiff(names(DescrVarObj[["Total"]]), cat_names)

    all_names <- c(summary_stat_names, cat_names)

    DescrVarObj[["Total"]][sapply(DescrVarObj[["Total"]], is.null)] <-
      NA
    tot <- c(NA_real_, unlist(DescrVarObj[["Total"]]))


    if (!is.null(DescrVarObj[["variable_options"]][["label"]])) {
      label <- DescrVarObj[["variable_options"]][["label"]]
    } else{
      label <- DescrVarObj[["variable_name"]]
    }
    DescrVarObj[["label"]] <- label

    tibl <- tibble(Variable = c(label,
                                all_names))

    length_tibl <- nrow(tibl)
    groups <- get_groupNames(DescrVarObj)

    for (group in groups) {
      DescrVarObj[[group]][sapply(DescrVarObj[[group]], is.null)] <- NA
      tmp <- c(NA_real_, unlist(DescrVarObj[[group]]))
      tibl %<>% bind_cols(!!group := tmp)
    }
    tibl %<>% bind_cols(Total = tot)

    p <-
      c(DescrVarObj[["test_list"]]$p, rep(NA_real_, length_tibl - 1))
    tibl %<>% bind_cols(p = p)

    test_name <-
      c(DescrVarObj[["test_list"]]$test_name, rep(NA_real_, length_tibl - 1))
    tibl %<>% bind_cols(Test = test_name)

    if (length(groups) == 2) {
      if (length(cat_names) == 2) {
        CI_upper <-
          c(DescrVarObj[["test_list"]]$CI[1], rep(NA_real_, length_tibl - 1))
        CI_lower <-
          c(DescrVarObj[["test_list"]]$CI[2], rep(NA_real_, length_tibl - 1))

        tibl %<>% bind_cols(CI_upper = CI_upper, CI_lower = CI_lower)
      } else{
        tibl %<>% bind_cols(
          CI_upper = rep(NA_real_, length_tibl),
          CI_lower = rep(NA_real_, length_tibl)
        )
      }
    }





    return(list(
      summary_list = DescrVarObj,
      length = length_tibl,
      tibble = tibl
    ))
  }


#' Create subtables for continuous variables which will comprise the output table
#'
#' @param DescrVarObj
#' @param var_name
#'
#' @return
#' @export
#'
#' @examples
#'
#' @import dplyr
#' @import magrittr
#' @import tibble
#' @import forcats
#' @import stringr
#' @import flextable
#' @import kableExtra
create_numeric_subtable.cont_summary <-
  function(DescrVarObj,
           var_name,
           format_options,
           format_summary_stats,
           format_p) {
    summary_stat_names <- names(DescrVarObj[["Total"]])

    DescrVarObj[["Total"]][sapply(DescrVarObj[["Total"]], is.null)] <-
      NA
    tot <- c(NA_real_, unlist(DescrVarObj[["Total"]]))

    if (!is.null(DescrVarObj[["variable_options"]][["label"]])) {
      label <- DescrVarObj[["variable_options"]][["label"]]
    } else{
      label <- DescrVarObj[["variable_name"]]
    }
    DescrVarObj[["label"]] <- label

    tibl <- tibble(Variable = c(label,
                                summary_stat_names))

    length_tibl <- nrow(tibl)
    groups <- get_groupNames(DescrVarObj)

    for (group in groups) {
      DescrVarObj[[group]][sapply(DescrVarObj[[group]], is.null)] <- NA
      tmp <- c(NA_real_, unlist(DescrVarObj[[group]]))
      tibl %<>% bind_cols(!!group := tmp)
    }
    tibl %<>% bind_cols(Total = tot)
    p <-
      c(DescrVarObj[["test_list"]]$p, rep(NA_real_, length_tibl - 1))
    tibl %<>% bind_cols(p = p)

    test_name <-
      c(DescrVarObj[["test_list"]]$test_name, rep(NA_real_, length_tibl - 1))

    tibl %<>% bind_cols(Test = test_name)

    if (length(groups) == 2) {
      CI_upper <-
        c(DescrVarObj[["test_list"]]$CI[1], rep(NA_real_, length_tibl - 1))
      CI_lower <-
        c(DescrVarObj[["test_list"]]$CI[2], rep(NA_real_, length_tibl - 1))

      tibl %<>% bind_cols(CI_upper = CI_upper, CI_lower = CI_lower)
    }

    return(list(
      summary_list = DescrVarObj,
      length = length_tibl,
      tibble = tibl
    ))

  }


#' S3 dispatcher for subtable creation
#'
#' @param DescrVarObj
#' @param var_name
#'
#' @return
#' @export
#'
#' @examples
#'
#' @import dplyr
#' @import magrittr
#' @import tibble
#' @import forcats
#' @import stringr
#' @import flextable
#' @import kableExtra
create_character_subtable <- function(DescrVarObj, ...) {
  UseMethod("create_character_subtable")
}


#' Create subtables for continuous variables which will comprise the output table
#'
#' @param DescrVarObj
#' @param var_name
#'
#' @return
#' @export
#'
#' @examples
#'
#' @import dplyr
#' @import magrittr
#' @import tibble
#' @import forcats
#' @import stringr
#' @import flextable
#' @import kableExtra
create_character_subtable.cont_summary <-
  function(DescrVarObj,
           var_name,
           format_options,
           format_summary_stats,
           format_p) {
    # Remember: You may not have a group which contains "Total" or "test_list" as a level.
    groups <- get_groupNames(DescrVarObj)


    if (format_options[["omit_Nmiss_if_0"]] == T) {
      if (isTRUE(DescrVarObj[["Total"]][["Nmiss"]] == 0)) {
        DescrVarObj[["Total"]] <-
          DescrVarObj[["Total"]][setdiff(names(DescrVarObj[["Total"]]), "Nmiss")]

        for (group in groups) {
          DescrVarObj[[group]] <-
            DescrVarObj[["Total"]][setdiff(names(DescrVarObj[[group]]), "Nmiss")]
        }
      }
    }

    if (!is.null(DescrVarObj[["variable_options"]][["label"]])) {
      label <- DescrVarObj[["variable_options"]][["label"]]
    } else{
      label <- DescrVarObj[["variable_name"]]
    }
    DescrVarObj[["label"]] <- label


    all_summary_stats_missing <- T
    for (summary_stat in names(DescrVarObj[["Total"]])) {
      if (!is.na(DescrVarObj[["Total"]][[summary_stat]])) {
        all_summary_stats_missing <- F
      }
    }

    if (all_summary_stats_missing) {
      length_tibl <- 2
      tibl <- bind_cols(Variables = c(label, "-"))

      for (group in groups) {
        tibl %<>% bind_cols(!!group := c("", "-"))
      }

      tibl %<>% bind_cols(Total = c("", "All entries NA"))
      tibl %<>% bind_cols(p =  c("", "-"))
      tibl %<>% bind_cols(Test = c("", "-"))

      if (length(groups) == 2) {
        tibl %<>% bind_cols(CI = c("", "-"))
      }

    } else{
      summary_stat_names <-
        setdiff(names(DescrVarObj[["Total"]]), c("Q1", "Q3", "min", "max"))

      DescrVarObj[["Total"]][sapply(DescrVarObj[["Total"]], is.null)] <-
        NA_character_

      DescrVarObj[["Total"]] <-
        combine_two_elements_of_list(DescrVarObj[["Total"]], "Q1", "Q3", format_summary_stats[["Q"]])
      DescrVarObj[["Total"]] <-
        combine_two_elements_of_list(DescrVarObj[["Total"]], "min", "max", format_summary_stats[["minmax"]])

      for (summary_stat in summary_stat_names) {
        DescrVarObj[["Total"]][[summary_stat]] <-
          format_summary_stats[[summary_stat]](DescrVarObj[["Total"]][[summary_stat]])
      }
      tot <- DescrVarObj[["Total"]]


      display_names <-
        names(DescrVarObj[["Total"]])  %>% c(label, .)


      tibl <- bind_cols(Variables = display_names)
      length_tibl <- length(display_names)




      for (group in groups) {
        DescrVarObj[[group]][sapply(DescrVarObj[[group]], is.null)] <- NA
        DescrVarObj[[group]] <-
          combine_two_elements_of_list(DescrVarObj[[group]], "Q1", "Q3", format_summary_stats[["Q"]])
        DescrVarObj[[group]] <-
          combine_two_elements_of_list(DescrVarObj[[group]], "min", "max", format_summary_stats[["minmax"]])
        for (summary_stat in summary_stat_names) {
          DescrVarObj[[group]][[summary_stat]] <-
            format_summary_stats[[summary_stat]](DescrVarObj[[group]][[summary_stat]])
        }
        tibl %<>% bind_cols(!!group := c("", unlist(DescrVarObj[[group]])))
      }

      tibl %<>% bind_cols(Total = c("", unlist(tot)))
      tibl %<>% bind_cols(p =  c("",
                                 format_p(DescrVarObj[["test_list"]]$p),
                                 rep("", length_tibl -
                                       2)))
      tibl %<>% bind_cols(Test = c("",
                                   DescrVarObj[["test_list"]]$test_name,
                                   rep("", length_tibl -
                                         2)))

      if (length(groups) == 2) {
        if (is.null(DescrVarObj[["test_list"]][["CI"]])) {
          CI_name <- ""
          CI <- ""
        } else{
          CI_name <- DescrVarObj[["test_list"]][["CI_name"]]
          CI <-
            paste0(
              "[",
              format_summary_stats[["CI"]](DescrVarObj[["test_list"]][["CI"]][1]),
              ", ",
              format_summary_stats[["CI"]](DescrVarObj[["test_list"]][["CI"]][2]),
              "]"
            )
        }
        tibl %<>% bind_cols(CI = c("",
                                   CI_name,
                                   CI,
                                   rep("", length_tibl -
                                         3)))
      }
    }

    return(list(
      summary_list = DescrVarObj,
      length = length_tibl,
      tibble = tibl
    ))
  }

#' Title
#'
#' @return
#' @export
#'
#' @examples
#'
#' @import dplyr
#' @import magrittr
#' @import tibble
#' @import forcats
#' @import stringr
#' @import flextable
#' @import kableExtra
create_character_subtable.cat_summary <-
  function(DescrVarObj,
           var_name,
           format_options,
           format_summary_stats,
           format_p) {
    ## Remember: Category levels may not be names "N"
    cat_names <- DescrVarObj[["variable_levels"]]
    cat_names_nonmissing <- setdiff(cat_names, "(Missing)")
    summary_stat_names <-
      setdiff(names(DescrVarObj[["Total"]]), cat_names)
    DescrVarObj[["Total"]][sapply(DescrVarObj[["Total"]], is.null)] <-
      "0 (0%)"

    N_total <- sum(unlist(DescrVarObj[["Total"]][cat_names]))

    if (format_options[["make_missing_a_category"]] == T) {
      N_nonmissing <- N_total
    } else{
      N_nonmissing <-
        sum(unlist(DescrVarObj[["Total"]][cat_names_nonmissing]))
    }

    for (summary_stat in summary_stat_names) {
      DescrVarObj[["Total"]][[summary_stat]] <-
        format_summary_stats[[summary_stat]](DescrVarObj[["Total"]][[summary_stat]])
    }

    for (summary_stat in cat_names_nonmissing) {
      DescrVarObj[["Total"]][[summary_stat]] <-
        paste0(DescrVarObj[["Total"]][[summary_stat]],
               " (",
               scales::label_percent()(DescrVarObj[["Total"]][[summary_stat]] / N_nonmissing)  ,
               ")")
    }

    if ("(Missing)" %in% cat_names) {
      DescrVarObj[["Total"]][["(Missing)"]] <-
        paste0(DescrVarObj[["Total"]][["(Missing)"]],
               " (",
               scales::label_percent()(DescrVarObj[["Total"]][[summary_stat]] / N_total)  ,
               ")")
    }


    tot <- c("", unlist(DescrVarObj[["Total"]]))

    if (!is.null(DescrVarObj[["variable_options"]][["label"]])) {
      label <- DescrVarObj[["variable_options"]][["label"]]
    } else{
      label <- DescrVarObj[["variable_name"]]
    }
    DescrVarObj[["label"]] <- label

    tibl <- tibble(Variables = c(label,
                                 summary_stat_names, cat_names))
    length_tibl <- nrow(tibl)
    groups <- get_groupNames(DescrVarObj)

    for (group in groups) {
      DescrVarObj[[group]][sapply(DescrVarObj[[group]], is.null)] <-
        "0 (0%)"


      N_group_total <-
        sum(unlist(DescrVarObj[[group]][cat_names]))

      if (format_options[["make_missing_a_category"]] == T) {
        N_group_nonmissing <- N_group_total
      } else{
        N_group_nonmissing <-
          sum(unlist(DescrVarObj[[group]][cat_names_nonmissing]))
      }


      for (summary_stat in summary_stat_names) {
        DescrVarObj[[group]][[summary_stat]] <-
          format_summary_stats[[summary_stat]](DescrVarObj[[group]][[summary_stat]])
      }

      for (summary_stat in cat_names_nonmissing) {
        DescrVarObj[[group]][[summary_stat]] <-
          paste0(
            DescrVarObj[[group]][[summary_stat]],
            " (",
            scales::label_percent()(DescrVarObj[[group]][[summary_stat]] / N_group_nonmissing),
            ")"
          )
      }

      if ("(Missing)" %in% cat_names) {
        DescrVarObj[[group]][["(Missing)"]] <-
          paste0(
            DescrVarObj[[group]][["(Missing)"]],
            " (",
            scales::label_percent()(DescrVarObj[[group]][["(Missing)"]] / N_group_total),
            ")"
          )
      }

      tmp <- c("", unlist(DescrVarObj[[group]]))
      tibl %<>% bind_cols(!!group := tmp)
    }
    tibl %<>% bind_cols(Total = tot)

    tibl %<>% bind_cols(p =  c("",
                               format_p(DescrVarObj[["test_list"]]$p),
                               rep("", length_tibl -
                                     2)))
    tibl %<>% bind_cols(Test = c("",
                                 DescrVarObj[["test_list"]]$test_name,
                                 rep("", length_tibl -
                                       2)))


    if (length(groups) == 2) {
      if (is.null(DescrVarObj[["test_list"]][["CI"]])) {
        CI_name <- ""
        CI <- ""
      } else{
        CI_name <- DescrVarObj[["test_list"]][["CI_name"]]
        CI <-
          paste0(
            "[",
            format_summary_stats[["CI"]](DescrVarObj[["test_list"]][["CI"]][1]),
            ", ",
            format_summary_stats[["CI"]](DescrVarObj[["test_list"]][["CI"]][2]),
            "]"
          )
      }
      tibl %<>% bind_cols(CI = c("",
                                 CI_name,
                                 CI,
                                 rep("", length_tibl -
                                       3)))
    }



    return(list(
      summary_list = DescrVarObj,
      length = length_tibl,
      tibble = tibl
    ))
  }


#' Does what the function name says
#'
#' @param list
#' @param elem1
#' @param elem2
#'
#' @return
#' @export
#'
#' @examples
#'
#' @import dplyr
#' @import magrittr
#' @import tibble
#' @import forcats
#' @import stringr
#' @import flextable
#' @import kableExtra
combine_two_elements_of_list <-
  function(lst, elem1, elem2, format_summary_stats) {
    if (c(elem1, elem2) %in% names(lst) %>% all()) {
      lst[[elem1]] <-
        paste0(format_summary_stats(lst[[elem1]]),
               " -- ",
               format_summary_stats(lst[[elem2]]))
      names(lst)[names(lst) == elem1] <- paste0(elem1, " - ", elem2)
      lst <- lst[setdiff(names(lst), elem2)]
    }
    else{
      return(lst)
    }
  }

#' Title
#'
#' @param DescrVarObj
#'
#' @return
#' @export
#'
#' @examples
#'
#' @import dplyr
#' @import magrittr
#' @import tibble
#' @import forcats
#' @import stringr
#' @import flextable
#' @import kableExtra
get_groupNames <- function(DescrVarObj) {
  setdiff(
    names(DescrVarObj),
    c(
      "Total",
      "test_list",
      "variable_name",
      "variable_levels",
      "variable_options",
      "label"
    )
  )
}



#' Title
#'
#' @param var
#'
#' @return
#' @export
#'
#' @examples
.N <- function(var) {
  sum(!is.na(var))
}

#' Title
#'
#' @param var
#'
#' @return
#' @export
#'
#' @examples
.Nmiss <- function(var) {
  sum(is.na(var))
}

#' Title
#'
#' @param var
#'
#' @return
#' @export
#'
#' @examples
.mean <- function(var) {
  mean(var, na.rm = T)
}

.sd <- function(var) {
  sd(var, na.rm = T)
}

.median <- function(var) {
  median(var, na.rm = T)
}

#' Title
#'
#' @param var
#'
#' @return
#' @export
#'
#' @examples
.Q1 <- function(var) {
  quantile(var, probs = 0.25, na.rm = T)
}

#' Title
#'
#' @param var
#'
#' @return
#' @export
#'
#' @examples
.Q3 <- function(var) {
  quantile(var, probs = 0.75, na.rm = T)
}



#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
.min <- function(var) {
  min(var, na.rm = T)
}


#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
.max <- function(var) {
  max(var, na.rm = T)
}


.factormean <- function(var) {
  var %>% as.character() %>% as.numeric() %>% mean(na.rm = T)
}


escape_latex_symbols <- function(tibl) {
  for (i in 1:nrow(tibl)) {
    for (j in 1:ncol(tibl)) {
      tibl[i, j] <- str_replace_all(tibl[i, j], fixed("%"), fixed("\\%"))
      tibl[i, j] <-
        str_replace_all(tibl[i, j], fixed("$"), fixed("\\$"))
      # tibl[i,j] <- str_replace_all(tibl[i,j], fixed("_"), fixed("\\_"))
    }
  }
  tibl
}


suggest_good_pagebreaks <- function(tibl) {
  for (i in 1:nrow(tibl)) {
    for (j in 1:ncol(tibl)) {
      tibl[i, j] <- str_replace_all(tibl[i, j], fixed("%"), fixed("\\%"))
      tibl[i, j] <-
        str_replace_all(tibl[i, j], fixed("$"), fixed("\\$"))
      # tibl[i,j] <- str_replace_all(tibl[i,j], fixed("_"), fixed("\\_"))
    }
  }
  tibl
}


sanitize_latex <- function(str_vec) {
  str_replace_all(str_vec,
                  "^\\s\\s\\-",
                  fixed("\\\\vphantom{padding} \\\\vphantom{padding} -"))
}


#' Title
#'
#' @param test_names
#'
#' @return
#' @export
#'
#' @examples
#'
#' @import dplyr
#' @import magrittr
#' @import tibble
#' @import forcats
#' @import stringr
#' @import flextable
#' @import kableExtra
create_test_abbreviations <- function(test_names) {
  erg <- character()
  for (test in test_names) {
    abbrev <- switch(
      test,
      `Cochrans Q test` = "CocQ",
      `McNemars test` = "McN",
      `Chi-squared goodness-of-fit test` = "chi1",
      `Pearsons chi-squared test` = "chi2",
      `Exact McNemars test` = "eMcN",
      `Boschloos test` = "Bolo",
      `Friedman test` = "Frie",
      `Wilcoxon two-sample signed-rank test` = "Wil2",
      `Wilcoxon one-sample signed-rank test` = "Wil1",
      `Mann–Whitney U test` = "MWU",
      `Kruskal–Wallis one-way ANOVA` = "KW",
      `Students paired t-test` = "tpar",
      `Mixed model ANOVA` = "MiAn",
      `Students one-sample t-test` = "tt1",
      `Welchs two-sample t-test` = "tt2",
      `F-test (ANOVA)` = "F",
      "Unknown test"
    )

    erg <- c(erg, abbrev)
  }
  erg
}

test_names <- c(
  "Cochrans Q test",
  "McNemars test",
  "Chi-squared goodness-of-fit test",
  "Pearsons chi-squared test",
  "Exact McNemars test",
  "Boschloos test",
  "Friedman test",
  "Wilcoxon two-sample signed-rank test",
  "Wilcoxon one-sample signed-rank test",
  "Mann–Whitney U test",
  "Kruskal–Wallis one-way ANOVA",
  "Students paired t-test",
  "Mixed model ANOVA",
  "Students one-sample t-test",
  "Welchs two-sample t-test",
  "F-test (ANOVA)"
)



#' Title
#'
#' @param var
#' @param group
#' @param test_options
#' @param test
#'
#' @return
#' @export
#'
#' @examples
#'
#' @import dplyr
#' @import magrittr
#' @import tibble
#' @import forcats
#' @import stringr
#' @import flextable
#' @import kableExtra
test_cont <-
  function(var, group, test_options, test = NULL) {
    # decide how to handle missings
    if (!is.null(group)) {
      tibl <- tibble(var = var, group = group)
      if (!test_options[["include_group_missings_in_test"]]) {
        tibl %<>% filter(group != "(Missing)")
      }

      var <- tibl %>% pull(var)
      group <- tibl %>% pull(group)

      n_levels_group <- length(levels(group))
    } else{
      n_levels_group <- 1
    }

    # if test is not supplied, determine test
    if (is.null(test)) {
      if (isTRUE(test_options[["nonparametric"]] == T)) {
        # ordinal variable
        if (isTRUE(test_options[["paired"]] == T)) {
          # ordinal variable, paired test
          if (n_levels_group == 2) {
            test <- "Wilcoxon two-sample signed-rank test"
          } else if (n_levels_group >= 2) {
            test <- "Friedman test"
          }
        } else{
          # ordinal variable, independent test
          if (is.null(group)) {
            test <- "Wilcoxon one-sample signed-rank test"
          } else if (n_levels_group == 2) {
            test <- "Mann–Whitney U test"
          } else if (n_levels_group >= 2) {
            test <- "Kruskal–Wallis one-way ANOVA"
          }
        }
      } else{
        # continuous variable
        if (isTRUE(test_options[["paired"]] == T)) {
          # continuous variable, paired test
          if (n_levels_group == 2) {
            test <- "Students paired t-test"
          } else{
            test <- "Mixed model ANOVA"
          }
        } else{
          # continuous variable, independent test
          if (n_levels_group == 1) {
            test <- "Students one-sample t-test"
          } else if (n_levels_group == 2) {
            test <- "Welchs two-sample t-test"
          } else if (n_levels_group >= 3) {
            test <- "F-test (ANOVA)"
          }
        }
      }
    }
    erg <- switch(
      test,
      `Wilcoxon two-sample signed-rank test` = {
        tibl <- tibble(var = var,
                       group = group,
                       id = test_options[["indices"]])
        level1 <- levels(group)[1]
        level2 <- levels(group)[2]

        x <-
          tibl %>% filter(group == level1) %>% arrange(id) %>% pull(var)
        y <-
          tibl %>% filter(group == level2) %>% arrange(id) %>% pull(var)

        list(p = stats::wilcox.test(x, y, paired = T)$p.value)
      },
      `Friedman test` = {
        list(p = stats::friedman.test(var ~ group |
                                        test_options[["indices"]])$p.value)
      },
      `Wilcoxon one-sample signed-rank test` = {
        list(p = stats::wilcox.test(var)$p.value)
      },
      `Mann–Whitney U test` = {
        tl <- stats::wilcox.test(var ~ group, conf.int = T)
        list(p = tl$p.value,
             CI = tl$conf.int,
             CI_name = "HL CI")
      },
      `Kruskal–Wallis one-way ANOVA` = {
        list(p = stats::kruskal.test(var ~ group)$p.value)
      },
      `Students paired t-test` = {
        tibl <- tibble(var = var,
                       group = group,
                       id = test_options[["indices"]])
        level1 <- levels(group)[1]
        level2 <- levels(group)[2]

        x <-
          tibl %>% filter(group == level1) %>% arrange(id) %>% pull(var)
        y <-
          tibl %>% filter(group == level2) %>% arrange(id) %>% pull(var)

        tl <- stats::t.test(x, y, paired = T)

        list(p = tl$p.value,
             CI = tl$conf.int,
             CI_name = "Mean dif. CI")
      },
      `Mixed model ANOVA` = {
        fit <- nlme::lme(var ~ group, random = ~ 1 | var.ind)
        tl <- nlme::anova.lme(fit)
        pv <- tl$`p-value`[2]
        list(p = pv)
      },
      `Students one-sample t-test` = {
        list(p = stats::t.test(var)$p.value)
      },
      `Welchs two-sample t-test` = {
        tl <- stats::t.test(var ~ group, var.equal = F)
        list(p = tl$p.value,
             CI = tl$conf.int,
             CI_name = "Mean dif. CI")
      },
      `F-test (ANOVA)` = {
        tl <- summary(stats::aov(var ~ group))[[1]]
        pv <- tl$`Pr(>F)`[1]
        list(p = pv)
      },
      list(p = NA_real_)
    )
    erg[["test_name"]] <- test
    erg
  }


#' Title
#'
#' @param var
#' @param group
#' @param test_options
#' @param test
#'
#' @return
#' @export
#'
#' @examples
#'
#' @import dplyr
#' @import magrittr
#' @import tibble
#' @import forcats
#' @import stringr
#' @import flextable
#' @import kableExtra
test_cat <-
  function(var, group, test_options, test = NULL) {
    # decide how to handle missings
    if (!is.null(group)) {
      tibl <- tibble(var = var, group = group)
      if (!isTRUE(test_options[["include_group_missings_in_test"]])) {
        tibl %<>% filter(group != "(Missing)")
      }
      if (!isTRUE(test_options[["include_categorical_missings_in_test"]])) {
        tibl %<>% filter(var != "(Missing)")
      }
      var <- tibl %>% pull(var)
      group <- tibl %>% pull(group)

      n_levels_group <- length(levels(group))
      n_levels_var <- length(levels(var))
    } else{
      tibl <- tibble(var = var)

      if (!isTRUE(test_options[["include_categorical_missings_in_test"]])) {
        tibl %<>% filter(var != "(Missing)")
      }

      var <- tibl %>% pull(var)

      n_levels_group <- 1
      n_levels_var <- length(levels(var))
    }


    # if test is not supplied, determine test
    if (is.null(test)) {
      if (is.ordered(var)) {
        # ordinal variable
        if (isTRUE(test_options[["paired"]] == T)) {
          # ordinal variable, paired test
          if (is.null(test_options("indices"))) {
            stop(
              "You need to supply patient IDs, e.g. via test_options=list(indices=patIDs)."
            )
          } else{
            if (n_levels_group == 2) {
              test <- "Wilcoxon two-sample signed-rank test"
            } else if (n_levels_group >= 2) {
              test <- "Friedman test"
            }
          }
        } else{
          # ordinal variable, independent test
          if (n_levels_group == 1) {
            test <- "Wilcoxon one-sample signed-rank test"
          } else if (n_levels_group == 2) {
            test <- "Mann–Whitney U test"
          } else if (n_levels_group >= 2) {
            test <- "Kruskal–Wallis one-way ANOVA"
          }
        }
      } else{
        # nominal variable
        if (isTRUE(test_options[["exact"]] == T)) {
          # nominal variable, exact test
          if (isTRUE(test_options[["paired"]] == T)) {
            # nominal variable, exact paired test
            if (n_levels_group == 2 & n_levels_var == 2) {
              test <- "Exact McNemars test"
            } else{
              test <- "No appropriate test available."
            }
          } else{
            # nominal variable, exact independent test
            if (n_levels_group == 2 & n_levels_var == 2) {
              test <- "Boschloos test"
            } else{
              test <- "No appropriate test available."
            }
          }
        } else{
          # nominal variable, asymptotic test
          if (isTRUE(test_options[["paired"]] == T)) {
            # nominal variable, asymptotic paired test
            if (n_levels_group == 2 & n_levels_var == 2) {
              test <- "McNemars test"
            } else if (n_levels_group >= 3 & n_levels_var == 2) {
              if (is.null(test_options("indices"))) {
                stop(
                  "You need to supply patient IDs, e.g. via test_options=list(indices=patIDs)."
                )
              } else{
                test <- "Cochrans Q test"
              }
            } else{
              test <- "No appropriate test available."
            }
          } else{
            # nominal variable, asymptotic independent test
            if (is.null(group) | n_levels_group == 1) {
              test <- "Chi-squared goodness-of-fit test"
            }
            else if (n_levels_group >= 2) {
              test <- "Pearsons chi-squared test"
            }
          }
        }
      }
    }

    erg <-
      switch(
        test,
        `Wilcoxon two-sample signed-rank test` = {
          tibl <- tibble(var = as.numeric(as.character(var)),
                         group = group,
                         id = test_options[["indices"]])

          level1 <- levels(group)[1]
          level2 <- levels(group)[2]
          x <-
            tibl %>% filter(group == level1) %>% arrange(id) %>% pull(var)

          y <-
            tibl %>% filter(group == level2) %>% arrange(id) %>% pull(var)

          list(p = stats::wilcox.test(x, y, paired = T)$p.value)
        },
        `Friedman test` = {
          list(p = stats::friedman.test(as.numeric(as.character(var)) ~ group |
                                          test_options[["indices"]])$p.value)
        },
        `Wilcoxon one-sample signed-rank test` = {
          list(p = stats::wilcox.test(as.numeric(as.character(var)))$p.value)
        },
        `Mann–Whitney U test` = {
          tl <-
            stats::wilcox.test(as.numeric(as.character(var)) ~ group, conf.int = T)
          list(p = tl$p.value,
               CI = tl$conf.int,
               CI_name = "HL CI")
        },
        `Kruskal–Wallis one-way ANOVA` = {
          list(p = stats::kruskal.test(as.numeric(as.character(var)) ~ group)$p.value)
        },
        `Exact McNemars test` = {
          list(p = exact2x2::mcnemar.exact(var, group)$p.value)
        },
        `Boschloos test` = {
          tabl <- table(var, group)
          x1 <- tabl[1, 1]
          n1 <- sum(tabl[, 1])
          x2 <- tabl[1, 2]
          n2 <- sum(tabl[, 2])
          list(
            p = exact2x2::boschloo(x1, n1, x2, n2)$p.value,
            CI = prop.test(table(var, group))$conf.int,
            CI_name = "Prop. dif. CI"
          )
        },
        `McNemars test` = {
          list(
            p = mcnemar.test(var, group)$p.value,
            CI = prop.test(table(var, group))$conf.int,
            CI_name = "Prop. dif. CI"
          )
        },
        `Cochrans Q test` = {
          list(p = DescTools::CochranQTest(var ~ group |
                                             test_options[["indices"]])$p.value)
        },
        `Chi-squared goodness-of-fit test` = {
          list(p = chisq.test(table(var))$p.value)
        },
        `Pearsons chi-squared test` = {
          if (n_levels_group == 2 & n_levels_var == 2) {
            list(
              p = chisq.test(var, group)$p.value,
              CI = prop.test(table(var, group))$conf.int,
              CI_name = "Prop. dif. CI"
            )
          } else{
            list(p = chisq.test(var, group)$p.value)
          }
        },
        list(p = NA_real_)
      )

    erg[["test_name"]] <- test
    erg
  }

