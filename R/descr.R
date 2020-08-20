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
                               ...)
      } else if (is.numeric(var)) {
        # Analyze continuous variable
        var_descr <- descr_cont(var,
                                group_var,
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
  function(var, group) {
    erg <- list()
    var_levels <- levels(var)


    for (group_name in levels(group)) {
      # Subset values for the respective group
      var_grp <- var[which(group == group_name)]
      cat_list <- list()
      cat_list[["N"]] <- .N(var_grp)
      for (cat_name in var_levels) {
        cat_list[[cat_name]] <- sum(var_grp == cat_name)
      }
      erg[[group_name]] <- cat_list
    }

    # Caclulate summary for whole cohort
    cat_list <- list()
    cat_list[["N"]] <- .N(var)
    for (cat_name in var_levels) {
      cat_list[[cat_name]] <- sum(var == cat_name)
    }
    erg[["Total"]] <- cat_list


    # Calculate test
    erg[["test_list"]] <- test_cat(var, group)

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

    attr(erg, "class") <- c("cont_summary", "list")
    erg
  }


#' S3 override for print function for DescrList objects
#'
#' @param DescrListObj
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
  switch(
    printFormat,
    tex = print_tex(),
    flex = print_flex(),
    print_console(DescrListObj, ...)
  )
}

print_console <- function(DescrListObj,
                          n = NULL,
                          width = NULL,
                          n_extra = NULL,
                          print_red_NA = F) {
  var_names <- names(DescrListObj[["variables"]])
  group_names <- c(DescrListObj[["group"]][["var"]] %>% levels(),
                   "Total")

  tibl <- bind_cols(Variable = character())
  for (group_name in group_names) {
    tibl %<>% bind_cols(!!group_name := numeric())
  }
  tibl %<>% bind_cols(p = numeric(),
                     Test = character())

  for (var_name in var_names) {
    sub_tibl <-
      DescrListObj[["variables"]][[var_name]] %>% create_subtable(., var_name)
    tibl %<>% bind_rows(sub_tibl)
  }

  print_format <- format(tibl,
                         n = n,
                         width = width,
                         n_extra = n_extra)
  if (print_red_NA) {
    print_format %>% cli::cat_line()
  } else{
    print_format %>% str_replace_all(pattern = fixed("\033[31mNA\033[39m"),
                                     fixed("\033[31m  \033[39m")) %>%  cli::cat_line()
  }

  invisible(tibl)
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
create_subtable <- function(DescrVarObj, ...) {
  UseMethod("create_subtable")
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
create_subtable.cat_summary <- function(DescrVarObj, var_name) {
  tot <- DescrVarObj[["Total"]]
  summary_stat_names <- names(tot)
  tot[sapply(tot, is.null)] <- NA
  tot <- c(NA_real_, unlist(tot))

  tibl <- tibble(Variable = c(var_name,
                             summary_stat_names))
  length_tibl <- nrow(tibl)

  groups <- setdiff(names(DescrVarObj), c("Total", "test_list"))

  for (group in groups) {
    tmp <- DescrVarObj[[group]]
    tmp[sapply(tmp, is.null)] <- NA
    tmp <- c(NA_real_, unlist(tmp))
    tibl %<>% bind_cols(!!group := tmp)
  }
  tibl %<>% bind_cols(Total = tot)

  p <-
    c(DescrVarObj[["test_list"]]$p, rep(NA_real_, length_tibl - 1))
  tibl %<>% bind_cols(p = p)

  test_name <-
    c(DescrVarObj[["test_list"]]$test_name, rep(NA_real_, length_tibl - 1))
  tibl %<>% bind_cols(Test = test_name)
  tibl
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
create_subtable.cont_summary <-
  function(DescrVarObj, var_name) {
    tot <- DescrVarObj[["Total"]]
    summary_stat_names <- names(tot)
    tot[sapply(tot, is.null)] <- NA
    tot <- c(NA_real_, unlist(tot))

    tibl <- tibble(Variable = c(var_name,
                               summary_stat_names))
    length_tibl <- nrow(tibl)

    groups <- setdiff(names(DescrVarObj), c("Total", "test_list"))

    for (group in groups) {
      tmp <- DescrVarObj[group]
      tmp[sapply(tmp, is.null)] <- NA
      tmp <- c(NA_real_, unlist(tmp))
      tibl %<>% bind_cols(!!group := tmp)
    }
    tibl %<>% bind_cols(Total = tot)
    p <-
      c(DescrVarObj[["test_list"]]$p, rep(NA_real_, length_tibl - 1))
    tibl %<>% bind_cols(p = p)

    test_name <-
      c(DescrVarObj[["test_list"]]$test_name, rep(NA_real_, length_tibl - 1))
    tibl %<>% bind_cols(Test = test_name)
    tibl
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
create_subtable.cont_summary_character <-
  function(DescrVarObj, var_name) {



    tot <- DescrVarObj[["Total"]]
    summary_stat_names <- names(tot)
    tot[sapply(tot, is.null)] <- NA

    tot <- combine_two_elements_of_list(tot, "Q1", "Q3")
    tot <- combine_two_elements_of_list(tot, "min", "max")


    for (name in names(tot)){
      tot[[name]] <- formatC(tot[[name]])
    }

    # display_names <- names(tot) %>% paste("  -", .) %>% c(var_name, .)
    display_names <- names(tot)  %>% c(var_name, .)

    tibl <- bind_cols(Variables = display_names)
    length_tibl <- length(display_names)

    # Remember: You may not have a group which contains "Total" or "test_list" as a level.
    groups <- setdiff(names(DescrVarObj), c("Total", "test_list"))
    grp_vars <- DescrVarObj[groups]

    for (group in groups) {
      grp_vars[[group]][sapply(grp_vars[[group]], is.null)] <- NA
      grp_vars[[group]] <- combine_two_elements_of_list(grp_vars[[group]], "Q1", "Q3")
      grp_vars[[group]] <- combine_two_elements_of_list(grp_vars[[group]], "min", "max")
      for (name in names(grp_vars[[group]])){
        grp_vars[[group]][[name]] <- formatC(grp_vars[[group]][[name]])
      }
      tibl %<>% bind_cols(!!group := c("", unlist(grp_vars[[group]]) ))
    }

    tibl %<>% bind_cols(Total = c("", unlist(tot)  ))
    tibl %<>% bind_cols(p =  c(formatC(DescrVarObj[["test_list"]]$p), rep("", length_tibl-1)))
    tibl %<>% bind_cols(Test = c(formatC(DescrVarObj[["test_list"]]$test_name), rep("", length_tibl-1)))


    ## TODO: put this in right place
    tibl %>% format() %>% .[-c(1,3)] %>%  str_replace_all(pattern = fixed('"'),
                                       fixed(' ')) %>%  cli::cat_line()

    invisible(tibl)
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
combine_two_elements_of_list <- function(lst, elem1, elem2){
  if (c(elem1, elem2) %in% names(lst) %>% all()){
    lst[[elem1]] <- paste0(formatC(lst[[elem1]]), " -- ", formatC(lst[[elem2]]))
    names(lst)[names(lst)==elem1] <- paste0(elem1, " - ", elem2)
    lst <- lst[setdiff(names(lst), elem2)]
  }
  else{
    return(lst)
  }
}








#' Test categorical variables
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
test_cat <- function(...) {
  return(list(p = 0.5, test_name = "t.test"))
}

#' Test continuous variables
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
test_cont <- function(var, group, ...) {
  return(list(p = 0.5, test_name = "t.test"))
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

iris_test <- iris %>% bind_cols(cat_var = c("a", "b") %>% sample(150, T) ) %>% as_tibble()
abc <- descr(iris_test, "Species")
bb <- create_subtable.cont_summary_character(abc$variables$Sepal.Length, "Sepal.Length")


absanitize_latex <- function(str_vec){
  str_replace_all(str_vec,  "^\\s\\s\\-", fixed("\\\\vphantom{padding} \\\\vphantom{padding} -"))
}

bb %>% xtable::xtable() %>% print(include.rownames=F, tabular.environment = "longtable", floating=F, sanitize.text.function=sanitize_latex)
bb %>% kable(format="latex", longtable=T)




bb %>% kbl(format="latex", longtable = T, booktabs = T, linesep = "") %>% kable_styling() %>%  footnote(alphabet = c("test", "yo")) %>%
  pack_rows(index=c(" " = 0, "Group 1" = 4, "Group 2" = 2))







#
# {
#   if (is.null(nonparametric)) {
#     nonparametric <- rep(F, ncol(dat))
#   }
#   if (is.null(t.log)) {
#     t.log <- rep(F, ncol(dat))
#   }
#   l.i <- 0
#
#   testings <- c()
#   pos <- c()
#   pos.i <- 0
#   pos.mult <- 1
#   index_var <- c()
#
#   datmiss <- matrix(NA, nrow = nrow(dat), ncol = ncol(dat))
#   group.na.index <- which(is.na(group))
#
#   if (group.miss) {
#     groupmiss <- as.numeric(group)
#     groupmiss[group.na.index] <- "NA"
#     datmiss <- dat
#   }
#   if (length(group.na.index) != 0) {
#     warning(
#       paste(
#         "Missing values in the group variable ( Observations: ",
#         list(group.na.index),
#         " )! Observations will be removed!"
#       )
#     )
#     dat <- dat[-group.na.index, , drop = FALSE]
#     group <- group[-group.na.index]
#   }
#
#   lgr <- length(levels(group))
#
#   n.or.miss_original <- n.or.miss
#
#
#   for (i in 1:ncol(dat)) {
#     gr.miss <- c()
#
#     n.or.miss <- n.or.miss_original
#     if (adaptive.miss & !anyNA(dat[[i]])) {
#       if ("miss.cat" %in% n.or.miss) {
#         n.or.miss <- n.or.miss[-which(n.or.miss == "miss.cat")]
#       }
#       if ("miss" %in% n.or.miss) {
#         n.or.miss <- n.or.miss[-which(n.or.miss == "miss")]
#       }
#     }
#
#
#     if (all(is.na(dat[[i]]))) {
#       if (!(missing(var.names))) {
#         if (data.names == T) {
#           var.n1 <- var.names[i]
#           var.n2 <-
#             paste(paste("(", names(dat)[i], sep = ""), ")", sep = "")
#           var.n <- paste(var.n1, var.n2, sep = " ")
#         } else {
#           var.n <- var.names[i]
#         }
#       } else {
#         var.n <- names(dat)[i]
#       }
#       ab <-
#         matrix(c(var.n, "", rep(
#           c("-", ""), (1 + length(levels(group)) + group.miss + 1++p.values + 3 - 1)
#         )),
#         nrow = 2)
#       ab <- as.data.frame(ab)
#     }
#     else if (is.factor(dat[[i]])) {
#       a <- table(dat[[i]], group)
#       if (group.miss & length(group.na.index) != 0) {
#         gr.miss <-
#           c(gr.miss, table(datmiss[, i], groupmiss)[, which(colnames(table(datmiss[, i], groupmiss)) == "NA")])
#       }
#       if (group.miss & length(group.na.index) == 0) {
#         gr.miss <- rep(0, length(levels(dat[[i]])))
#       }
#       d <- table(dat[[i]])
#       if (percent.vertical == T) {
#         b <- prop.table(as.matrix(table(dat[[i]], group)), 2)
#         e <- prop.table(as.matrix(table(dat[[i]])), 2)
#         ab <- data.frame(cbind(a[, 1], b[, 1] * 100))
#         if (ncol(a) >= 2) {
#           for (k in 2:ncol(a)) {
#             ab <- data.frame(cbind(ab, a[, k], b[, k] * 100))
#           }
#         }
#         if (group.miss) {
#           ab <-
#             data.frame(cbind(
#               ab,
#               as.vector(d) + gr.miss,
#               as.vector(prop.table((
#                 as.matrix(table(dat[[i]])) + gr.miss
#               ), 2)) * 100
#             ))
#         } else {
#           ab <- data.frame(cbind(ab, as.vector(d), as.vector(e) * 100))
#         }
#
#
#         if (missing(var.names)) {
#           digits.p <-
#             c(digits.p, rep(digits.p[length(digits.p)], length = length(names(dat))))
#         }
#         else {
#           digits.p <-
#             c(digits.p, rep(digits.p[length(digits.p)], length = length(var.names)))
#         }
#
#
#
#         if (create == "word" |
#             create == "R" | create == "archive") {
#           for (j in seq(1, 2 * (length(levels(group))) + 1, by = 2)) {
#             for (k in 1:nrow(ab)) {
#               if (ab[k, j] != 0) {
#                 ab[k, j] <-
#                   paste(ab[k, j], " (", formatr(ab[k, (j + 1)], digits.p[i]), "%)", sep = "")
#               } else {
#                 ab[k, j] <- paste(ab[k, j], " ( - ) ")
#               }
#             }
#           }
#         } else {
#           for (j in seq(1, 2 * (length(levels(group))) + 1, by = 2)) {
#             for (k in 1:nrow(ab)) {
#               ab[k, j] <-
#                 paste(ab[k, j], " (", formatr(ab[k, (j + 1)], digits.p[i]), "\\%)", sep = "")
#             }
#           }
#         }
#       } else {
#         b <- prop.table(as.matrix(table(dat[[i]], group)), 1)
#         ab <- data.frame(cbind(a[, 1], b[, 1] * 100))
#         if (ncol(a) >= 2) {
#           for (k in 2:ncol(a)) {
#             ab <- data.frame(cbind(ab, a[, k], b[, k] * 100))
#           }
#         }
#         if (group.miss) {
#           ab <- data.frame(cbind(ab, as.vector(d) + gr.miss))
#         } else {
#           ab <- data.frame(cbind(ab, as.vector(d)))
#         }
#         if (create == "word" |
#             create == "R" | create == "archive") {
#           for (j in seq(1, 2 * (length(levels(group))), by = 2)) {
#             for (k in 1:nrow(ab)) {
#               ab[k, j] <-
#                 paste(ab[k, j], " (", formatr(ab[k, (j + 1)], digits.p[i]), "%)", sep = "")
#             }
#           }
#         } else {
#           for (j in seq(1, 2 * (length(levels(group))), by = 2)) {
#             for (k in 1:nrow(ab)) {
#               ab[k, j] <-
#                 paste(ab[k, j], " (", formatr(ab[k, (j + 1)], digits.p[i]), "\\%)", sep = "")
#             }
#           }
#         }
#       }
#
#       index.delete <- (1:(length(levels(group)) + 1)) * 2
#       ab <- ab[, -index.delete]
#
#       if ("miss.cat" %in% n.or.miss) {
#         miss.end <- c()
#         for (j in 1:lgr) {
#           miss.j <-
#             length(which(is.na(dat[which(group == levels(group)[j]), i])))
#           miss.end <- c(miss.end, miss.j)
#         }
#         miss.all <- length(which(is.na(dat[[i]])))
#         ab <- rbind(ab, c(miss.end, miss.all))
#         if (group.miss) {
#           gr.miss <-
#             c(gr.miss, length(which(is.na(datmiss[which(groupmiss == "NA"), i]))))
#         }
#       }
#
#       ab <- rbind(rep("", lgr + 1), ab, rep("", lgr + 1))
#       if (group.miss) {
#         gr.miss <- c("", gr.miss, "")
#       }
#
#       if (!(missing(var.names))) {
#         if (data.names == T) {
#           var.n1 <- var.names[i]
#           var.n2 <-
#             paste(paste("(", names(dat)[i], sep = ""), ")", sep = "")
#           var.n <- paste(var.n1, var.n2, sep = " ")
#         } else {
#           var.n <- var.names[i]
#         }
#       } else {
#         var.n <- names(dat)[i]
#       }
#       ab <- cbind(NA, ab)
#
#
#       ab <- as.data.frame(ab)
#       levels(dat[[i]]) <- paste(" ", levels(dat[[i]]))
#       if (create == "R") {
#         levels(dat[[i]]) <- paste("- ", levels(dat[[i]]))
#       }
#       if ("miss.cat" %in% n.or.miss) {
#         if (create == "R") {
#           ab[, 1] <- c(var.n, levels(dat[[i]]), "- Missing", "")
#         } else {
#           ab[, 1] <- c(var.n, levels(dat[[i]]), "  Missing", "")
#         }
#       } else {
#         ab[, 1] <- c(var.n, levels(dat[[i]]), "")
#       }
#       if (create != "word" &
#           create != "R" & create != "archive") {
#         for (j in 1:(length(levels(dat[[i]])) + ("miss.cat" %in% n.or.miss))) {
#           ab[j + 1, 1] <- paste("\\hspace{2ex}", ab[j + 1, 1])
#         }
#       } else {
#         for (j in 1:(length(levels(dat[[i]])) + ("miss.cat" %in% n.or.miss))) {
#           ab[j + 1, 1] <- paste(" ", ab[j + 1, 1])
#         }
#       }
#       if (group.miss) {
#         ab <- cbind(ab, gr.miss)
#       }
#
#       pvalues_var <- matrix(F, ncol = ncol(dat))
#       pvalues_var[i] <- p.values
#       if (pvalues_var[i]) {
#         a <- dat[[i]]
#         a.list <- list()
#         a.list[[(length(levels(group)) + 1)]] <- stats::na.omit(a)
#         n.vector <- c()
#         for (k in 1:length(levels(group))) {
#           a.list[[k]] <- stats::na.omit(a[which(group == levels(group)[k])])
#           n.vector <- c(n.vector, length(a.list[[k]]))
#         }
#         for (l in 1:length(levels(group))) {
#           if (n.vector[l] == 0) {
#             if (group.non.empty) {
#               pvalues_var[i] <- F
#             }
#             else {
#               warning(
#                 paste0(
#                   "For variable ",
#                   var.n,
#                   " a group has no observations for that variable.
#                              This group will be dropped for p-value calculations."
#                 )
#               )
#             }
#           }
#           else if (n.vector[l] < group.min.size) {
#             pvalues_var[i] <- F
#           }
#         }
#
#
#         if (length(table(factor(dat[[i]]))) <= 1) {
#           warning(
#             paste0(
#               "Variable ",
#               var.n,
#               " has less than 2 non-empty categories. No p-value calculations will be performed."
#             )
#           )
#           pvalues_var[i] <- F
#         } else {
#           for (l in 1:length(table(dat[[i]]))) {
#             if (table(dat[[i]])[l] == 0) {
#               if (cat.non.empty) {
#                 pvalues_var[i] <- F
#               }
#               else {
#                 warning(
#                   paste0(
#                     "Variable ",
#                     var.n,
#                     " Has an empty category.
#                                This category will be dropped for p-value calculations."
#                   )
#                 )
#               }
#             }
#           }
#         }
#
#         if (pvalues_var[i]) {
#           if (index) {
#             m <-
#               m.cat(
#                 dat[[i]],
#                 group,
#                 paired = paired,
#                 is.ordered = is.ordered(dat[[i]]),
#                 default.unordered.unpaired.test
#               )
#             if (!(m %in% testings)) {
#               testings <- c(testings, m)
#               l.i <- l.i + 1
#               index.i <- letters[l.i]
#             }
#             else {
#               index.i <- letters[which(testings == m)[1]]
#             }
#           } else {
#             index.i <- c()
#           }
#           p_list <- p.cat(
#             dat[[i]],
#             group,
#             paired = paired,
#             is.ordered = is.ordered(dat[[i]]),
#             correct.cat = correct.cat,
#             correct.wilcox = correct.wilcox,
#             index = index.i,
#             create = create,
#             default.unordered.unpaired.test
#           )
#           pv <- p_list$pv.formatted
#           index_var[i] <- T
#         } else {
#           index_var[i] <- F
#           pv <- "--"
#           p_list <-
#             list(
#               p.value = "--",
#               test.value = "--",
#               test.name = "--"
#             )
#         }
#         ab <- cbind(ab,
#                     c("", pv, rep(
#                       "", length(levels(dat[[i]])) + ("miss.cat" %in% n.or.miss)
#                     )))
#       }
#       else {
#         p_list <- list(
#           p.value = "--",
#           test.value = "--",
#           test.name = "--"
#         )
#       }
#
#       ab <- cbind(
#         ab,
#         c("", p_list$p.value, rep(
#           "", length(levels(dat[[i]])) + ("miss.cat" %in% n.or.miss)
#         )),
#         c("", p_list$test.value, rep(
#           "", length(levels(dat[[i]])) + ("miss.cat" %in% n.or.miss)
#         )),
#         c("", p_list$test.name, rep(
#           "", length(levels(dat[[i]])) + ("miss.cat" %in% n.or.miss)
#         ))
#       )
#
#       # pos.i.alt <- pos.i
#       #
#       # pos.i <- pos.i + length(levels(dat[[i]])) + 2 + length(n.or.miss)
#     } else {
#       a <- dat[[i]]
#       a.list <- list()
#       a.list[[(length(levels(group)) + 1)]] <- stats::na.omit(a)
#
#       n.vector <- c()
#       for (k in 1:length(levels(group))) {
#         a.list[[k]] <- stats::na.omit(a[which(group == levels(group)[k])])
#         n.vector <- c(n.vector, length(a.list[[k]]))
#       }
#       n.vector <- c(n.vector, length(stats::na.omit(a)))
#
#       n.miss <- c()
#       for (k in 1:length(levels(group))) {
#         miss.k <- which(is.na(a[which(group == levels(group)[k])]))
#         n.miss <- c(n.miss, length(miss.k))
#       }
#       n.miss <- c(n.miss, length(which(is.na(a))))
#
#       ab <-
#         matrix(NA, nrow = 6, ncol = (length(levels(group)) + 1))
#
#       if (group.miss) {
#         a.miss <- datmiss[, i]
#         a.miss <- stats::na.omit(a.miss)
#         a.list.miss <-
#           stats::na.omit(a.miss[which(groupmiss == "NA")])
#
#         n.vector.miss <- length(a.list.miss)
#
#         miss.k.miss <-
#           which(is.na(a.miss[which(groupmiss == "NA")]))
#         n.miss.miss <- c(length(miss.k.miss))
#
#         n.vector <- c(n.vector, n.vector.miss)
#         n.miss <- c(n.miss, n.miss.miss)
#         a.list[[length(levels(group)) + 2]] <- a.list.miss
#         ab <-
#           matrix(NA, nrow = 6, ncol = (length(levels(group)) + 2))
#       }
#
#       if ("n" %in% n.or.miss) {
#         ab[1, ] <- n.vector
#       }
#       if (!("n" %in% n.or.miss) & "miss" %in% n.or.miss) {
#         ab[1, ] <- n.miss
#       }
#       for (d in 1:length(a.list)) {
#         if (length(a.list[[d]]) != 0) {
#           ab[2, d] <- formatr(mean(a.list[[d]]), digits.m)
#           ab[3, d] <- formatr(stats::sd(a.list[[d]]), digits.sd)
#         } else {
#           ab[2, d] <- "-"
#           ab[3, d] <- "-"
#         }
#       }
#       ab[4, ] <-
#         sapply(a.list, med.new, simplify = T, k = digits.qu)
#       ab[5, ] <-
#         sapply(
#           a.list,
#           inqur,
#           simplify = T,
#           k = digits.qu,
#           q.type = q.type
#         )
#       ab[6, ] <-
#         sapply(a.list, minmax, simplify = T, k = digits.minmax)
#
#       if ("n" %in% n.or.miss & "miss" %in% n.or.miss) {
#         ab <- rbind(ab[1, ], n.miss, ab[2:6, ])
#       }
#
#       row.names(ab) <- NULL
#
#       if (group.miss) {
#         ab <-
#           rbind(rep("", (length(
#             levels(group)
#           ) + 2)), ab, rep("", (length(
#             levels(group)
#           ) + 2)))
#       } else {
#         ab <-
#           rbind(rep("", (length(
#             levels(group)
#           ) + 1)), ab, rep("", (length(
#             levels(group)
#           ) + 1)))
#       }
#
#       if (!(missing(var.names))) {
#         if (data.names == T) {
#           var.n1 <- var.names[i]
#           var.n2 <-
#             paste(paste("(", names(dat)[i], sep = ""), ")", sep = "")
#           var.n <- paste(var.n1, var.n2, sep = " ")
#         } else {
#           var.n <- var.names[i]
#         }
#       } else {
#         var.n <- names(dat)[i]
#       }
#
#       if (create == "word") {
#         row.ab <- c()
#         if ("n" %in% n.or.miss) {
#           row.ab <- c(row.ab, "    N")
#         }
#         if ("miss" %in% n.or.miss) {
#           row.ab <- c(row.ab, "    Missing")
#         }
#         if (!("n" %in% n.or.miss) & !("miss" %in% n.or.miss)) {
#           row.ab <- c(row.ab, "  ")
#         }
#         row.ab <-
#           c(row.ab, "    Mean", "    SD", "    Median", "    Q1 -- Q3", "    Min. -- Max.")
#       } else if (create == "R") {
#         row.ab <- c()
#         if ("n" %in% n.or.miss) {
#           row.ab <- c(row.ab, "    - N")
#         }
#         if ("miss" %in% n.or.miss) {
#           row.ab <- c(row.ab, "   - Missing")
#         }
#         if (!("n" %in% n.or.miss) & !("miss" %in% n.or.miss)) {
#           row.ab <- c(row.ab, "  ")
#         }
#         row.ab <-
#           c(row.ab,
#             "    - Mean",
#             "    - SD",
#             "    - Median",
#             "    - Q1 -- Q3",
#             "    - Min. -- Max.")
#       }
#       else if (create == "archive") {
#         row.ab <- c()
#         if ("n" %in% n.or.miss) {
#           row.ab <- c(row.ab, "-N")
#         }
#         if ("miss" %in% n.or.miss) {
#           row.ab <- c(row.ab, "-Missing")
#         }
#         if (!("n" %in% n.or.miss) & !("miss" %in% n.or.miss)) {
#           row.ab <- c(row.ab, "  ")
#         }
#         row.ab <-
#           c(row.ab,
#             "-Mean",
#             "-SD",
#             "-Median",
#             "-Q1 -- Q3",
#             "-Min. -- Max.")
#       } else {
#         row.ab <- c()
#         if ("n" %in% n.or.miss) {
#           row.ab <- c(row.ab, "    \\hspace{2ex} N ")
#         }
#         if ("miss" %in% n.or.miss) {
#           row.ab <- c(row.ab, "\\hspace{2ex} Missing")
#         }
#         if (!("n" %in% n.or.miss) & !("miss" %in% n.or.miss)) {
#           row.ab <- c(row.ab, "  ")
#         }
#         row.ab <- c(
#           row.ab,
#           "\\hspace{2ex} Mean",
#           "\\hspace{2ex} SD",
#           "\\hspace{2ex} Median",
#           "\\hspace{2ex} Q1 -- Q3",
#           "\\hspace{2ex} Min. -- Max."
#         )
#       }
#
#       ab <- as.data.frame(ab)
#       ab <- cbind(NA, ab)
#       ab[, 1] <- c(var.n, row.ab, "")
#
#       pvalues_var <- matrix(F, ncol = ncol(dat))
#       pvalues_var[i] <- p.values
#       if (pvalues_var[i]) {
#         for (l in 1:length(levels(group))) {
#           if (n.vector[l] == 0) {
#             if (group.non.empty) {
#               pvalues_var[i] <- F
#             }
#             else {
#               warning(
#                 paste0(
#                   "For variable ",
#                   var.n,
#                   " a group has no observations for that variable.
#                              This group will be dropped for p-value calculations."
#                 )
#               )
#             }
#           }
#           else if (n.vector[l] < group.min.size) {
#             pvalues_var[i] <- F
#           }
#         }
#         if (length(table(dat[[i]])) <= 1) {
#           pvalues_var[i] <- F
#         } else {
#           for (l in 1:length(table(dat[[i]]))) {
#             if (table(dat[[i]])[l] == 0) {
#               pvalues_var[i] <- F
#             }
#           }
#         }
#
#         if (pvalues_var[i]) {
#           if (index) {
#             m <- m.cont(
#               group,
#               paired = paired,
#               is.ordered = is.ordered(dat[[i]]),
#               nonparametric = nonparametric[i],
#               t.log = t.log[i]
#             )
#             if (!(m %in% testings)) {
#               testings <- c(testings, m)
#               l.i <- l.i + 1
#               index.i <- letters[l.i]
#             }
#             else {
#               index.i <- letters[which(testings == m)[1]]
#             }
#           } else {
#             index.i <- c()
#           }
#           p_list <- p.cont(
#             dat[[i]],
#             group,
#             paired = paired,
#             is.ordered = is.ordered(dat[[i]]),
#             nonparametric = nonparametric[i],
#             t.log = t.log[i],
#             var.equal = var.equal,
#             index = index.i,
#             create = create
#           )
#           pv <- p_list$pv.formatted
#           index_var[i] <- T
#         } else {
#           index_var[i] <- F
#           pv <- "--"
#           p_list <-
#             list(
#               p.value = "--",
#               test.value = "--",
#               test.name = "--"
#             )
#         }
#         if (!("miss" %in% n.or.miss) & !("n" %in% n.or.miss)) {
#           ab <- cbind(ab,
#                       c("", pv, rep(
#                         "", 5 + ("n" %in% n.or.miss & "miss" %in% n.or.miss)
#                       )))
#           ab <- ab[-2, ]
#         } else {
#           ab <- cbind(ab,
#                       c("", pv, rep(
#                         "", 6 + ("n" %in% n.or.miss & "miss" %in% n.or.miss)
#                       )))
#         }
#       }
#       else {
#         p_list <- list(
#           p.value = "--",
#           test.value = "--",
#           test.name = "--"
#         )
#       }
#
#       if (!("miss" %in% n.or.miss) & !("n" %in% n.or.miss)) {
#         ab <- cbind(
#           ab,
#           c("", p_list$p.value, rep(
#             "", 5 + ("n" %in% n.or.miss & "miss" %in% n.or.miss)
#           )),
#           c("", p_list$test.value, rep(
#             "", 5 + ("n" %in% n.or.miss & "miss" %in% n.or.miss)
#           )),
#           c("", p_list$test.name, rep(
#             "", 5 + ("n" %in% n.or.miss & "miss" %in% n.or.miss)
#           ))
#         )
#         ab <- ab[-2, ]
#       } else {
#         ab <- cbind(
#           ab,
#           c("", p_list$p.value, rep(
#             "", 6 + ("n" %in% n.or.miss & "miss" %in% n.or.miss)
#           )),
#           c("", p_list$test.value, rep(
#             "", 6 + ("n" %in% n.or.miss & "miss" %in% n.or.miss)
#           )),
#           c("", p_list$test.name, rep(
#             "", 6 + ("n" %in% n.or.miss & "miss" %in% n.or.miss)
#           ))
#         )
#       }
#     }
#     names(ab) <- 1:length(ab)
#
#     if (i == 1) {
#       ab1 <- ab
#     } else {
#       ab1 <- rbind(ab1, ab)
#     }
#
#     pos.i.alt <- pos.i
#     pos.i <- nrow(ab1)
#
#     if (!silent) {
#       print(list(
#         "i" = i,
#         "pos.i" = pos.i,
#         "ab1" = ab1
#       ))
#     }
#     if (landscape == F) {
#       if (is.null(pos.pagebr)) {
#         pos.pagebr <- 50
#       }
#     } else {
#       if (is.null(pos.pagebr)) {
#         pos.pagebr <- 30
#       }
#     }
#     if (pos.i > pos.pagebr * pos.mult & i <= ncol(dat)) {
#       pos <- c(pos, pos.i.alt)
#       pos.mult <- pos.mult + 1
#     }
#   }
#   return(
#     list(
#       "descr" = ab1,
#       "pos" = pos,
#       "pos.pagebr" = pos.pagebr,
#       "testings" = testings,
#       "pvalues_var" = pvalues_var
#     )
#   )
# }
