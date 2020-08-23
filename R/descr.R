library(tidyverse)
library(magrittr)
library(kableExtra)

#' Create a descriptive statistics table
#'
#' Generate a table of descriptive statistics with p-values obtained in tests
#' for difference between the groups.
#'
#' @param dat
#' Data frame or tibble. The data set to be analyzed. Can contain continuous or factor (also ordered) variables.
#' @param group
#' Vector of the grouping variable.
#' @param var.names
#' Optional. Vector of names to be used in the table for the analyzed variables.
#' @param percent.vertical
#' Logical. Should "vertical" percentages for categorical variables be provided?
#' @param data.names
#' Logical. If \code{var.names} are specified, should names as saved in \code{dat} be added in brackets?
#' @param nonparametric
#' Logical or vector of indices. If logical / vector of indices then all / only
#' these continuous variables will be tested using non-parametric methods.
#' @param landscape
#' Logical. Should the table be in landscape? Only useful if you want create a
#'  "pdf"- or "knitr"-document in the following. (see \code{pos.pagebr})
#' @param pos.pagebr
#' Vector of positions of the pagebreak in tex (or pdf). This is a bit fuzzy.
#' It is the number of lines after a pagebreak should be done.\cr
#' If it is not specified, 45 will be used for "\code{landscape=FALSE}" and
#' 30 will be used for "\code{landscape=TURE}".\cr
#' Only useful if you want know the number for a pagebreak when you create
#' a "pdf"- or "knitr"-document in the following.
#' @param paired
#' Logical. Should paired tests be applied? The groups must have the same length.
#' @param var.equal
#' Logical. Should variances be assumed to be equal when applying t-tests?
#' @param correct.cat
#' Logical. Should correction be used in chi-sqared tests (see \code{\link{chisq.test}})
#' @param correct.wilcox
#' Logical. Should correction be used in wilcoxon tests (see \code{\link{wilcox.test}})
#' @param silent
#' Logical. Should intermediate stages be shown (more for technical reasons)?
#' @param p.values
#' Logical. Should calculate p-values? If you won't p-values \code{index} were set to \code{FALSE}.
#' @param group.min.size
#' For each variable, a p-value is only calculated if each non-empty group contains
#'  at least \code{group.min.size} observations for that variable.
#' @param group.non.empty
#' For each variable, a p-value is only calculated if each group contains
#' at least one observation for that variable.
#' @param cat.non.empty
#' For categorical variables a p-value is only calculated if each category is non-empty.
#' @param n.or.miss
#' Should the number of observations, missings for continuous variables, and/or
#' missings for categorical variables be provided ("n", "miss", "miss.cat")?
#' Combinations are allowed.
#' @param adaptive.miss
#' Should the missing row be automatically omitted if there are not missings?
#' @param group.miss
#' Logical. Schould add a column for the Missings in group?
#' @param t.log
#' Vector of indices: The variables for which the log of the original data should
#' be used when testing for a difference between the groups.
#' @param index
#' Logical. Should the tests used be labeled by footnotes? Only usefull if
#' "p-values" in \code{which.col}.
#' @param create
#' Which output document should be produced in the following step
#' (one of "pdf", "tex", "knitr", "word" or "R").
#' @param digits.m
#' Number of digits for presentation in the table: For mean.
#' @param digits.sd
#' Number of digits for presentation in the table: For standard deviation.
#' @param digits.qu
#' Vector of numbers of digits for presentation in the table: For quantiles
#' (if no value is specified it will be tried to provide a reasonable presentation).
#' @param digits.minmax
#' Number of digits for presentation in the table: For minimum and maximum.
#' @param digits.p
#' Vector with numbers of digits for presentation in the table: For percentages.
#' First vector element is number of digits for the first variable,
#' second element for second variable and so on.
#' @param q.type
#' Integer between 1 and 9 that selects a quantile algorithm.
#' @param default.unordered.unpaired.test
#' Any of c("Chisq", "Fisher_exact", "Fisher_boschloo").
#' Chooses the default test for categorical, unordered, unpaired variables.
#'
#' @return
#' Depending on the value of the create parameter either pdf, word, tex, R
#' or an file optimized for use in connection with knitr will be created containing
#' the descriptive statistics table with the speak for the document to create in the following.
#' For example you choose \code{create="pdf"} then the table is written in \code{TeX}-Code.
#' Attention: the table has no caption and numbers of observations per group.
#'
#' @author Lorenz Uhlmann, Csilla van Lunteren, Jan Meis
#'
#' @seealso
#' \code{\link{med.new}}\cr
#' \code{\link{inqur}}\cr
#' \code{\link{minmax}}\cr
#' \code{\link{f.r}}\cr
#' \code{\link{formatr}}\cr
#' \code{\link{m.cat}}\cr
#' \code{\link{m.cont}}\cr
#' \code{\link{p.cat}}\cr
#' \code{\link{p.cont}}\cr
#'
#' @examples
#' \dontrun{
#' }
#'
#' @importFrom dplyr select
#' @importFrom magrittr `%<>%`
#' @importFrom tibble as_tibble
#' @importFrom forcats as_factor
#'
#'
#' @importFrom flextable autofit
#' @importFrom flextable flextable
#'
descr <-
  function(dat,
           group = NULL,
           var.names = NULL,
           percent.vertical = T,
           data.names = T,
           nonparametric = c(),
           landscape = F,
           pos.pagebr = NULL,
           paired = F,
           var.equal = T,
           correct.cat = F,
           correct.wilcox = T,
           silent = T,
           p.values = T,
           group.min.size = F,
           group.non.empty = F,
           cat.non.empty = F,
           n.or.miss = "n",
           adaptive.miss = T,
           group.miss = F,
           t.log = c(),
           index = T,
           create = "knitr",
           digits.m = 1,
           digits.sd = 2,
           digits.qu = c(),
           digits.minmax = 1,
           digits.p = c(1),
           q.type = 2,
           default.unordered.unpaired.test = "Chisq",
           ...) {
    # Coerce dataset to tibble
    dat %<>% as_tibble(dat)

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
                               ...)
      } else if (is.numeric(var)) {
        # Analyze continuous variable
        var_descr <- descr_cont(var,
                                group_var,
                                var_name,
                                ...)
      } else{
        stop("Somehow, you have variables which are neither factors nor numerical.")
      }
      # Append result of analysis to list
      ergs[["variables"]][[var_name]] <- var_descr
    }
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
descr_cat <-
  function(var,
           group,
           var_name,
           summary_stats = c(N = .N, mean = .factormean)) {
    erg <- list()
    var_levels <- levels(var)


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


    # Calculate test
    erg[["test_list"]] <- test_cat(var, group)
    erg[["variable_name"]] <- var_name
    erg[["variable_levels"]] <- var_levels

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
descr_cont <-
  function(var,
           group,
           var_name,
           summary_stats = c(
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
           ...) {
    erg <- list()

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

    # Calculate test
    erg[["test_list"]] <- test_cont(var, group)
    erg[["variable_name"]] <- var_name

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
#' @importFrom tibble
print.DescrList <-  function(DescrListObj,
                             printFormat = options("DescrTabFormat")[[1]],
                             ...) {
  if (is.null(printFormat)) {
    printFormat <- "console"
  }

  DescrPrintObj <- create_printObj(DescrListObj, printFormat)

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

  print_list <- list()

  for (var_name in var_names) {
    print_list[[var_name]] <-
      DescrListObj[["variables"]][[var_name]] %>% create_subtable(., var_name)
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
  group_n <- c(group_n, sum(group_n))

  printObj[["group_n"]] <- group_n
  printObj[["group_names"]] <- group_names
  tibl <- tibble()

  for (var_name in var_names) {
    printObj[["variables"]][[var_name]] <-
      print_list[[var_name]][["summary_list"]]
    printObj[["lengths"]][[var_name]] <-
      print_list[[var_name]][["length"]]
    tibl %<>%  bind_rows(print_list[[var_name]][["tibble"]])
  }
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
#'
#' @examples
print_numeric <- function(DescrPrintObj,
                          n = NULL,
                          width = NULL,
                          n_extra = NULL,
                          print_red_NA = F) {
  print_format <- format(DescrPrintObj[["tibble"]],
                         n = n,
                         width = width,
                         n_extra = n_extra)
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
print_console <- function(DescrPrintObj,
                          n = NULL,
                          width = NULL,
                          n_extra = NULL,
                          print_red_NA = F) {
  tibl <- DescrPrintObj[["tibble"]]
  var_names <- names(DescrPrintObj[["variables"]])

  c1 <- tibl %>% pull(1)

  c1 <- ifelse(c1 %in% var_names, c1, paste0("  -", c1))
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
print_tex <- function(DescrPrintObj) {
  tibl <- DescrPrintObj[["tibble"]]
  var_names <- names(DescrPrintObj[["variables"]])
  lengths <- c(unlist(DescrPrintObj[["lengths"]]) - 1)
  group_names <- DescrPrintObj$group_names

  names(lengths) <- c(var_names)

  c1 <- tibl %>% pull(1)
  indx_varnames <- c1 %in% var_names


  tests <- tibl %>% filter(Test != "") %>% pull(Test) %>% unique()
  p_vec <- tibl %>% pull(p)
  p_indx <- which(p_vec != "")

  test_abbrev <- create_test_abbreviations(tests)


  for (idx in p_indx) {
    tibl[idx, "p"] %<>% paste0("\\textsuperscript{", test_abbrev[match(tibl[idx, "Test"], tests)]  , "}")
  }

  tibl %<>% select(-Test)

  alig <- paste0(c("l", rep("c", ncol(tibl) - 1)), collapse = "")
  alig2 <- paste0(c("l", rep("c", ncol(tibl) - 1)))
  actual_colnames <- names(tibl[!indx_varnames,])
  N_numbers <-
    c("", paste0("(N=", DescrPrintObj[["group_n"]], ")") , "")

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
    kableExtra::footnote(symbol = c(tests), symbol_manual = test_abbrev) %>%
    pack_rows(index = lengths) %>%
    add_header_above(actual_colnames, line = F, align = alig2) %>%
    capture.output()

  tex %<>% str_replace_all(fixed("\\\\"), fixed("\\\\*"))
  pagebreak_indices <-
    str_detect(tex, fixed("textbf")) %>% which() %>% tail(-1)
  if (length(pagebreak_indices) > 0) {
    tex[pagebreak_indices - 2] %<>% str_replace_all(fixed("\\\\*"), fixed("\\\\"))
  }


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
print_html <- function(DescrPrintObj) {
  tibl <- DescrPrintObj[["tibble"]]
  var_names <- names(DescrPrintObj[["variables"]])
  lengths <- c(unlist(DescrPrintObj[["lengths"]]) - 1)
  names(lengths) <- c(var_names)

  c1 <- tibl %>% pull(1)
  indx_varnames <- c1 %in% var_names


  tests <- tibl %>% filter(Test != "") %>% pull(Test) %>% unique()
  p_vec <- tibl %>% pull(p)
  p_indx <- which(p_vec != "")

  test_abbrev <- create_test_abbreviations(tests)

  for (idx in p_indx) {
    tibl[idx, "p"] %<>% paste0("^", test_abbrev[match(tibl[idx, "Test"], tests)]  , "^")
  }

  tibl %<>% select(-Test)


  alig <- paste0(c("l", rep("c", ncol(tibl) - 1)), collapse = "")
  alig2 <- paste0(c("l", rep("c", ncol(tibl) - 1)))
  actual_colnames <- names(tibl[!indx_varnames,])
  N_numbers <-
    c("", paste0("(N=", DescrPrintObj[["group_n"]], ")") , "")


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
    kableExtra::footnote(symbol = tests, symbol_manual = test_abbrev) %>%
    pack_rows(index = lengths) %>%
    add_header_above(actual_colnames, line = F, align = alig2) %>%
    cat()

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
#' @import flextable
print_word <- function(DescrPrintObj) {
  tibl <- DescrPrintObj[["tibble"]]
  var_names <- names(DescrPrintObj[["variables"]])
  lengths <- c(unlist(DescrPrintObj[["lengths"]]) - 1)
  names(lengths) <- c(var_names)

  c1 <- tibl %>% pull(1)
  indx_varnames <- c1 %in% var_names


  tests <- tibl %>% filter(Test != "") %>% pull(Test) %>% unique()
  p_vec <- tibl %>% pull(p)
  p_indx <- which(p_vec != "")


  tibl2 <- tibl %>% select(-Test)
  # tibl2[,1 ] <- ifelse(!indx_varnames, paste0("  ", tibl2 %>% pull(1)), tibl2 %>% pull(1))

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


  test_abbrev <- create_test_abbreviations(tests)

  for (test in tests) {
    ft %<>%  footnote(
      i =  which((tibl %>% pull(Test)) %in% test),
      j =  which(names(tibl2) == "p"),
      value = as_paragraph(c(test)),
      ref_symbols = c(test_abbrev[match(test, tests)]),
      part = "body"
    )
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
create_numeric_subtable.cat_summary <-
  function(DescrVarObj, var_name) {
    ## Remember: Category levels may not be names "N"
    cat_names <- DescrVarObj[["variable_levels"]]
    summary_stat_names <-
      setdiff(names(DescrVarObj[["Total"]]), cat_names)

    all_names <- c(summary_stat_names, cat_names)


    DescrVarObj[["Total"]][sapply(DescrVarObj[["Total"]], is.null)] <-
      NA
    tot <- c(NA_real_, unlist(DescrVarObj[["Total"]]))

    tibl <- tibble(Variable = c(var_name,
                                all_names))
    length_tibl <- nrow(tibl)

    groups <-
      setdiff(names(DescrVarObj),
              c("Total", "test_list", "variable_name"))

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
create_numeric_subtable.cont_summary <-
  function(DescrVarObj, var_name) {
    summary_stat_names <- names(DescrVarObj[["Total"]])

    DescrVarObj[["Total"]][sapply(DescrVarObj[["Total"]], is.null)] <-
      NA
    tot <- c(NA_real_, unlist(DescrVarObj[["Total"]]))

    tibl <- tibble(Variable = c(var_name,
                                summary_stat_names))

    length_tibl <- nrow(tibl)
    groups <-
      setdiff(names(DescrVarObj),
              c("Total", "test_list", "variable_name"))

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
create_character_subtable.cont_summary <-
  function(DescrVarObj, var_name) {
    summary_stat_names <- names(DescrVarObj[["Total"]])
    DescrVarObj[["Total"]][sapply(DescrVarObj[["Total"]], is.null)] <-
      NA_character_

    DescrVarObj[["Total"]] <-
      combine_two_elements_of_list(DescrVarObj[["Total"]], "Q1", "Q3")
    DescrVarObj[["Total"]] <-
      combine_two_elements_of_list(DescrVarObj[["Total"]], "min", "max")

    for (name in names(DescrVarObj[["Total"]])) {
      DescrVarObj[["Total"]][[name]] <-
        formatC(DescrVarObj[["Total"]][[name]])
    }
    tot <- DescrVarObj[["Total"]]

    # display_names <- names(tot) %>% paste("  -", .) %>% c(var_name, .)
    display_names <-
      names(DescrVarObj[["Total"]])  %>% c(var_name, .)

    tibl <- bind_cols(Variables = display_names)
    length_tibl <- length(display_names)

    # Remember: You may not have a group which contains "Total" or "test_list" as a level.
    groups <-
      setdiff(names(DescrVarObj),
              c("Total", "test_list", "variable_name"))
    grp_vars <- DescrVarObj[groups]

    for (group in groups) {
      DescrVarObj[[group]][sapply(DescrVarObj[[group]], is.null)] <- NA
      DescrVarObj[[group]] <-
        combine_two_elements_of_list(DescrVarObj[[group]], "Q1", "Q3")
      DescrVarObj[[group]] <-
        combine_two_elements_of_list(DescrVarObj[[group]], "min", "max")
      for (name in names(DescrVarObj[[group]])) {
        DescrVarObj[[group]][[name]] <-
          formatC(DescrVarObj[[group]][[name]])
      }
      tibl %<>% bind_cols(!!group := c("", unlist(DescrVarObj[[group]])))
    }

    tibl %<>% bind_cols(Total = c("", unlist(tot)))
    tibl %<>% bind_cols(p =  c("", scales::pvalue_format()(DescrVarObj[["test_list"]]$p), rep("", length_tibl -
                                                                                2)))
    tibl %<>% bind_cols(Test = c("", formatC(DescrVarObj[["test_list"]]$test_name), rep("", length_tibl -
                                                                                          2)))

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
create_character_subtable.cat_summary <-
  function(DescrVarObj, var_name) {
    ## Remember: Category levels may not be names "N"
    cat_names <- DescrVarObj[["variable_levels"]]
    summary_stat_names <-
      setdiff(names(DescrVarObj[["Total"]]), cat_names)
    DescrVarObj[["Total"]][sapply(DescrVarObj[["Total"]], is.null)] <-
      "0 (0%)"

    N_tot <- DescrVarObj[["Total"]][[1]]

    for (summary_stat in summary_stat_names) {
      DescrVarObj[["Total"]][[summary_stat]] <-
        formatC(DescrVarObj[["Total"]][[summary_stat]])
    }


    for (summary_stat in cat_names) {
      DescrVarObj[["Total"]][[summary_stat]] <-
        paste0(DescrVarObj[["Total"]][[summary_stat]],
               " (",
               scales::label_percent()(DescrVarObj[["Total"]][[summary_stat]] / N_tot)  ,
               ")")
    }


    tot <- c("", unlist(DescrVarObj[["Total"]]))

    tibl <- tibble(Variables = c(var_name,
                                 summary_stat_names, cat_names))
    length_tibl <- nrow(tibl)

    groups <-
      setdiff(names(DescrVarObj),
              c("Total", "test_list", "variable_name", "variable_levels"))

    for (group in groups) {
      DescrVarObj[[group]][sapply(DescrVarObj[[group]], is.null)] <-
        "0 (0%)"

      N_grp <- DescrVarObj[[group]][[1]]

      for (summary_stat in summary_stat_names) {
        DescrVarObj[[group]][[summary_stat]] <-
          formatC(DescrVarObj[[group]][[summary_stat]])
      }

      for (summary_stat in cat_names) {
        DescrVarObj[[group]][[summary_stat]] <-
          paste0(DescrVarObj[[group]][[summary_stat]],
                 " (",
                 scales::label_percent()(DescrVarObj[[group]][[summary_stat]] / N_grp)  ,
                 ")")
      }
      tmp <- c("", unlist(DescrVarObj[[group]]))
      tibl %<>% bind_cols(!!group := tmp)
    }
    tibl %<>% bind_cols(Total = tot)


    tibl %<>% bind_cols(p =  c("", scales::pvalue_format()(DescrVarObj[["test_list"]]$p), rep("", length_tibl -
                                                                                2)))
    tibl %<>% bind_cols(Test = c("", formatC(DescrVarObj[["test_list"]]$test_name), rep("", length_tibl -
                                                                                          2)))

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
combine_two_elements_of_list <- function(lst, elem1, elem2) {
  if (c(elem1, elem2) %in% names(lst) %>% all()) {
    lst[[elem1]] <-
      paste0(formatC(lst[[elem1]]), " -- ", formatC(lst[[elem2]]))
    names(lst)[names(lst) == elem1] <- paste0(elem1, " - ", elem2)
    lst <- lst[setdiff(names(lst), elem2)]
  }
  else{
    return(lst)
  }
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




#' p-value calculator for continous variables
#'
#' Calculate the p-value for continous variables.
#' The decision which test to use is equal to \code{\link{m.cont}}.
#' The p-value is calculated using one of the four tests:
#' Wilcoxon-Test, t-Test, Kruskal-Test, Anova.
#'
#' @param x
#' Vector of the continous variable.
#' @param group
#' Vector of the grouping variable.
#' @param paired
#' Logical. Is the categorial Variable paired?
#' @param is.ordered
#' Logical. Is the categorial Variable ordered?
#' @param nonparametric
#' Logical. Should the continuous variable tested by using non-parametric methods.
#' @param t.log
#' Logical. Should be used the log of the original data.
#' @param var.equal
#' Logical. Should variances be assumed to be equal when applying t-tests?
#' @param index
#' Optional. Label for the footnote.
#' The footnotes aren't produced in this function.
#' @param create
#' Which output document should be produced in the following step (one of "pdf", "tex", "knitr", or "word").
#'
#' @details
#' Wilcoxon Test: A nonparametric Test for a comparison of 2 dependent samples.
#' (see \code{\link{wilcox.test}}).
#' Mann-Whitney-U Test: A nonparametric Test for a comparison of 2 independent samples. (
#' see \code{\link{wilcox.test}}).
#' t-Test: A parametric Test for a comparison of 2 (in)dependent samples.
#' (see \code{\link{t.test}}).
#' Friedman-Test: A nonparametric Test for a comparison of more than 2 dependent samples.
#' (see \code{\link{friedman.test}}).
#' Anova Type III: A parametric Test for a comparison of more than 2 dependent samples.
#' (see \code{\link[car]{Anova}} with \code{}).
#' Kruskal-Wallis-Test: A nonparametric Test for a comparison of more than 2 independent samples.
#' (see \code{\link{kruskal.test}}).
#' Anova: A parametric Test for a comparison of more than 2 independent samples.
#' (see \code{\link{aov}}).
#'
#' @return
#' The p-value with index which test is ussed is returned.
#' author
#' Lorenz Uhlmann, Csilla van Lunteren
#'
#' @seealso
#' \link[nlme]{lme}\cr
#' \link[car]{Anova}\cr
#'
#' @examples
#' \dontrun{
#' p.cont(x = rnorm(100, 0, 1), group = rep(1:4, 25))
#' }
#'
#' @import lme4
#' @import SparseM
#' @importFrom MatrixModels model.Matrix
#' @importFrom  nlme lme
#' @importFrom car Anova
#'
test_cont <-
  function(var,
           group,
           paired = F,
           is.ordered = F,
           nonparametric = F,
           t.log = F,
           var.equal = F,
           index = c()) {
    group <- droplevels(group)

    if (length(levels(group)) == 2) {
      if (nonparametric) {
        test.name <- "Mann–Whitney U test"
        tl <- stats::wilcox.test(var ~ group, paired = paired)
        pv <- tl$p.value
        test.value <- tl$statistic
      } else {
        if (t.log) {
          test.name <- "log-t-test"
          var <- log(var)
        }
        test.name <- "Students t-test"
        tl <-
          stats::t.test(var ~ group, paired = paired, var.equal = var.equal)
        pv <- tl$p.value
        test.value <- tl$statistic
      }
    } else {
      if (paired) {
        # Annahme: Beobachtungen stehen pro "Gruppe" jeweils in derselben Reihenfolge untereinander!
        var.ind <-
          rep(1:(length(var) / length(levels(group))), length(levels(group)))
        if (nonparametric) {
          var.ind <-
            rep(1:(length(var) / length(levels(group))), length(levels(group)))
          test.name <- "Friedman"

          tl <- stats::friedman.test(var ~ group | var.ind)
          pv <- tl$p.value
          test.value <- tl$statistic
        } else {
          test.name <- "paired_lme_F-test(dont_really_know_what_happens_here)"
          fit <- nlme::lme(var ~ group, random = ~ 1 | var.ind)
          # pv <- car::Anova(fit, type = "III")[2, 3]
          tl <- nlme::anova.lme(fit)
          pv <- tl$`p-value`[2]
          test.value <- tl$`F-value`[2]
        }
      } else {
        if (nonparametric) {
          test.name <- "Kruskal"
          tl <- stats::kruskal.test(var ~ group)
          pv <- tl$p.value
          test.value <- tl$statistic
        } else {
          test.name <- "F-test"
          tl <- summary(stats::aov(var ~ group))[[1]]
          pv <- tl$`Pr(>F)`[1]
          test.value <- tl$`F value`[1]
        }
      }
    }


    list(
      p = pv,
      test_value = test.value,
      test_name = test.name
    )
  }





#' p-value calculator for categorical variables
#'
#' Calculate the p-value for categorial variables.
#' The decision which test to use is equal to \code{\link{m.cat}}.
#' The p-value is calculated using one of the three tests:
#' Wilcoxon-Test, McNemar-Test, Chi-Squared-Test.
#'
#' @param x
#' Vector of the categorial variable.
#' @param group
#' Vector of the grouping variable.
#' @param paired
#' Logical. Is the categorial Variable paired?
#' @param is.ordered
#' Logical. Is the categorial Variable ordered?
#' @param correct.cat
#' Logical. Should correction be used in chi-sqared tests (see \code{\link{chisq.test}})
#' @param correct.wilcox
#' Logical. Should correction be used in wilcoxon tests (see \code{\link{wilcox.test}})
#' @param index
#' Optional. Label for the footnote.
#' The footnotes aren't produced in this function.
#' @param create
#' Which output document should be produced in the following step
#' (one of "pdf", "tex", "knitr", or "word").
#' Only usefull if \code{index} is not \code{NULL}.
#'
#' @details
#' Wilcoxon-Test: A Test for a comparison of 2 (in)dependent, ordered samples.
#' (see \code{\link{wilcox.test}}).
#' Kruskal_wallis-Test: A Test for a comparison of more than 2 (in)dependent,
#' ordered samples. (see \code{\link{kruskal.test}}).
#' McNemar Test: A Test for a comparison of 2 dependent,
#' not ordered samples. (see \code{\link{mcnemar.test}}).
#' Chi-Squared Test: A Test for a comparison of 2 or more than 2 independent,
#' not ordered samples. (see \code{\link[DescTools]{CochranQTest}}).
#' Cochran's Q Test: A test for a comparison of 2 or more than 2 dependent,
#' not ordered samples. (see \code{\link{chisq.test}}).
#'
#' @return
#' The p-value with index which test is ussed is returned.
#'
#' @author
#' Lorenz Uhlmann, Csilla van Lunteren
#'
#' @examples
#' \dontrun{
#' p.cat(x = rep(1:5, 20), group = rep(1:4, 25))
#' }
#'
test_cat <-
  function(var,
           group,
           paired = F,
           is.ordered = F,
           correct.cat = F,
           correct.wilcox = T,
           index = c(),
           create = "tex",
           default.unordered.unpaired.test = "Chi-squared test") {
    group <- droplevels(group)
    var <- droplevels(var)

    if (is.ordered) {
      if (length(levels(group)) == 2) {
        test.name <- "Mann–Whitney U test"
        tl <-
          stats::wilcox.test(as.numeric(var) ~ group, paired = paired)
        pv <- tl$p.value
        test.value <- tl$statistic
      } else {
        test.name <- "Kruskal–Wallis one-way ANOVA"
        tl <- stats::kruskal.test(var ~ group)
        pv <- tl$p.value
        test.value <- tl$statistic
      }
    } else {
      if (paired) {
        if (length(levels(group)) == 2) {
          test.name <- "McNemars test"
          tl <-
            stats::mcnemar.test(table(var, group), correct = correct.cat)
          pv <- tl$p.value
          test.value <- tl$statistic
        } else {
          var.ind <-
            rep(1:(length(var) / length(levels(group))), length(levels(group)))
          test.name <- "Cochrans Q test"
          tl <- DescTools::CochranQTest(var ~ group | var.ind)
          pv <- tl$p.value
          test.value <- tl$statistic
        }
      } else {
        if (default.unordered.unpaired.test == "Chi-squared test") {
          test.name <- "Chi-squared test"
          tl <- stats::chisq.test(var, group, correct = correct.cat)
          pv <- tl$p.value
          test.value <- tl$statistic
        }
        else if (default.unordered.unpaired.test == "Boschloos test") {
          if ((nrow(table(var, group)) != 2) |
              (ncol(table(var, group)) != 2)) {
            warning(
              "Fisher_boschloo test not implemented for non-2x2 tables. Defaulting to Fisher_exact."
            )
            test.name <- "Fisher_exact"
            tl <- stats::fisher.test(var, group)
            pv <- tl$p.value
            test.value <- 0
          }
          else {
            test.name <- "Boschloos test"
            tl <-
              Exact::exact.test(table(group, var),
                                method = "boschloo",
                                to.plot = F)
            pv <- tl$p.value
            test.value <- tl$statistic
          }
        }
        else if (default.unordered.unpaired.test == "Fishers exact test") {
          test.name <- "Fishers exact test"
          tl <- stats::fisher.test(var, group)
          pv <- tl$p.value
          test.value <- 0
        }
      }
    }

    list(
      p = pv,
      test_value = test.value,
      test_name = test.name
    )
  }



create_test_abbreviations <- function(test_names){
  erg <- character()
  for (test in test_names){
    abbrev <- switch(test,
                     `Students t-test`="t",
                     `F-test`="F",
                     `Chi-squared test`="chi",
                     `Mann–Whitney U test`="MWU",
                     # TODO: fix this.
                     "add_rest_later")

    erg <- c(erg, abbrev)
  }
  erg
}
















