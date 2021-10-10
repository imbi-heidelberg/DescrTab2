utils::globalVariables("where")
utils::globalVariables(".")

#' Calculate descriptive statistics
#'
#' Generate a list of descriptive statistics. By default, the function calculates summary statistics such as mean,
#' standard deviation, quantiles, minimum and maximum for continuous variables and relative and absolute frequencies
#' for categorical variables. Also calculates p-values for an appropriately chosen statistical test.
#' For two-group comparisons, confidence intervals for appropriate summary measures of group differences are calculated aswell. In particular,
#' Wilson score intervals [1] from \link[stats]{prop.test} are used for categorical variables with 2 levels, confidence intervals from \link[stats]{t.test}
#' are used for continuous variables and confidence intervals for the Hodge-Lehman estimator [2] from \link[stats]{wilcox.test} are used for ordinal variables.
#'
#' @param dat
#' Data frame or tibble. The data set to be analyzed. Can contain continuous or factor (also ordered) variables.
#' @param group name (as character) of the group variable in dat.
#' @param group_labels named list of labels for the levels of the group variable in dat.
#' @param var_labels named list of variable labels.
#' @param var_options named list of lists. For each variable, you can have special options that apply only to that variable.
#' These options are specified in this argument. See the details and examples for more explanation.
#' @param summary_stats_cont named list of summary statistic functions to be used for numeric variables.
#' @param summary_stats_cat named list of summary statistic function to be used for categorical variables.
#' @param format_summary_stats named list of formatting functions for summary statistics.
#' @param format_p formatting function for p-values.
#' @param format_options named list of formatting options.
#' @param test_options named list of test options.
#' @param reshape_rows named list of lists. Describes how to combine different summary statistics into the same row.
#' @param ... further argument to be passed along
#'
#' @section Labels:
#' \code{group_labels} and \code{var_labels} need to be named lists of character elements. The names of the list elements have to match the variable
#' names in your dataset. The values of the list elements are the labels that will be assigned to these variables when printing.
#'
#' @section Custom summary statistics:
#' \code{summary_stats_cont} and \code{summary_stats_cat} are both named lists of functions. The names of the list elements are
#' what will be displayed in the leftmost column of the descriptive table. These functions should take a vector and return
#' a value. \cr
#' Each summary statistic has to have an associated formatting function in the \code{format_summary_stats} list.
#' The functions in \code{format_summary_stats} take a numeric value and convert it to a character string, e.g. 0.2531235 -> "0.2". \cr
#' The \code{format_p} function converts p-values to character strings, e.g. 0.05 -> "0.05" or 0.000001 -> "<0.001".
#'
#' @section Formatting options:
#' Further formatting options can be specified in the \code{format_options} list. It contains the following members:
#' \itemize{
#' \item{\code{print_Total}}{ (logical) controls whether to print the "Total" column. If print_Total = NULL, print_Total will be set
#' to TRUE if test_options$paired == FALSE, else it will be set to FALSE.}
#' \item{\code{print_p}}{ (logical) controls whether to print the p-value column.}
#' \item{\code{print_CI}}{ (logical) controls whether to print the confidence intervals for group-differences.}
#' \item{\code{combine_mean_sd}}{ (logical) controls whether to combine the mean and sd row into one mean ± sd row. This is a
#' shortcut argument for the specification of an appropriate entry in the \code{reshape_rows} argument.}
#' \item{\code{combine_median_Q1_Q3}}{ (logical) controls whether to combine the median, Q1 and Q3 row into one median (Q1, Q3) row. This is a
#' shortcut argument for the specification of an appropriate entry in the \code{reshape_rows} argument.}
#' \item{\code{omit_Nmiss_if_0}}{ (logical)  controls whether to omit the Nmiss row in continuous variables there are no missings in the variable.}
#' \item{\code{omit_missings_in_group}}{ (logical)  controls whether to omit all observations where the group variable is missing.}
#' \item{\code{percent_accuracy}}{ (numeric)  A number to round to. Use (e.g.) 0.01 to show 2 decimal places of precision. If NULL, the default, uses a heuristic that
#' should ensure breaks have the minimum number of digits needed to show the difference between adjacent values. See documentation of scales::label_percent}
#' \item{\code{percent_suffix}}{ (character) the symbol to be used where "\%" is appropriate, sensible choices are usually "\%" (default) or "" (i.e., empty string) }
#' \item{\code{row_percent}}{ (logical) controls wheter percentages of regular categorical variables should be calculated column-wise (default) or row-wise}
#' \item{\code{Nmiss_row_percent}}{ (logical) controls whether percentages of the "Nmiss"-statistic (number of missing values) should be calculated column-wise (default) or row-wise}
#' \item{\code{absolute_relative_frequency_mode}}{ (character)  controls how to display frequencies.
#' It may be set to one of the following options:
#' \itemize{
#' \item{\code{"both"}}{ will display absolute and relative frequencies.}
#' \item{\code{"only_absolute"}}{ will only display absolute frequencies. }
#' \item{\code{"only_relative"}}{ will only display relative frequencies. }
#'   }
#'  }
#' \item{\code{omit_missings_in_categorical_var}}{ (logical) controls whether to omit missing values in categorical variables completely.}
#' \item{\code{categorical_missing_percent_mode}}{ (character)  controls how to display percentages in categorical variables with a (Missing) category.
#' It may be set to one of the following options:
#' \itemize{
#' \item{\code{"no_missing_percent"}}{ omits a percentage in the missing category entirely.}
#' \item{\code{"missing_as_regular_category"}}{ treats (Missing) as a regular category for \%-calculation
#' This means that if You have three categories: "A" with 10 counts, "B" with 10 counts and "(Missing)" with 10 counts,
#' they will become "A": 10 (33\%), "B": 10 (33\%), "(Missing)": 10 (33\% purposes.)}
#'
#' \item{\code{"missing_as_separat_category"}}{ calculates (Missing) percentages with respect to
#' all observations (i.e. #(Missing) / N), but calculates all other catetgory percentages with respect to the non-missing
#' observations (e.g. #A / N_nonmissing). This means that if You have three categories: "A" with 10 counts, "B" with 10 counts
#' and "(Missing)" with 10 counts, they will become "A": 10 (50\%), "B": 10 (50\%), "(Missing)": 10 (33\%)}
#'
#' \item{\code{"caption"}}{ adds a table caption to the LaTeX, Word or PDf document}
#'   }
#'  }
#' \item{\code{replace_empty_string_with_NA}}{ (logical) controls whether empty strings ("") should be replaced
#' with missing value (\code{NA_character_}).}
#' }
#'
#'
#' @section Test options:
#' \code{test_options} is a named list with test options. It's members \code{paired},  \code{nonparametric}, and
#' \code{exact} (logicals) control which test in the corresponding situation. For details, check out the vignette:
#' \url{https://imbi-heidelberg.github.io/DescrTab2/articles/test_choice_tree_pdf.pdf}. The \code{test_options = list(test_override="<some test name>")} option can be specified to force usage of a
#' specific test. This will produce errors if the data does not allow calculation of that specific test, so be wary.
#' Use \code{print_test_names()} to see a list of all available test names. If \code{paired = TRUE} is specified, you need to supply an index variable
#' \code{indices} that specifies which datapoints in your dataset are paired. \code{indices} may either be a length one character vector that describes
#' the name of the index variable in your dataset, or a vector containing the respective indices.
#' If you have \code{guess_id} set to \code{TRUE} (the default), \code{DescrTab2} will try to guess
#' the ID variable from your dataset and report a warning if it succeedes.
#' See \url{https://imbi-heidelberg.github.io/DescrTab2/articles/usage_guide.html#Paired-observations-1}
#' for a bit more explanation. The optional list \code{additional_test_args} can be used to pass arguments along to test functions,
#' e.g. \code{additional_test_args=list(correct=TRUE)} will request continuity correction if available.
#'
#' @section Customization for single variables:
#' The \code{var_options} list can be used to conduct customizations that should only apply to a single variable and leave
#' the rest of the table unchanged. \cr
#' \code{var_options} is a list of named lists. This means that each member of \code{var_options} is itself a list again.
#' The names of the list elements of \code{var_options} determine the variables to which the options will apply.
#' Let's say you have an \code{age} variable in your dataset. To change 'descr' options only for \code{age}, you will need to pass
#' a list of the form \code{var_options = list(age = list(<Your options here>))}. \cr
#' You can replace \code{<Your options here>} with the following options:
#' \itemize{
#' \item{\code{label}}{ a character string containing the label for the variable}
#' \item{\code{summary_stats}}{ a list of summary statistics. See section "Custom summary statistics"}
#' \item{\code{format_summary_stats}}{ a list of formatting functions for summary statistics. See section "Custom summary statistics"}
#' \item{\code{format_p}}{ a function to format p-values. See section "Custom summary statistics"}
#' \item{\code{format_options}}{ a list of formatting options. See section "Formatting options"}
#' \item{\code{test_options}}{ a list of test options. See section "Test options"}
#' \item{\code{test_override}}{ manually specify the name of the test you want to apply. You can see a list of choices
#' by typing \code{print_test_names()}. Possible choices are:
#' \itemize{
#' \item{\code{"Cochran's Q test"}}{ }
#' \item{\code{"McNemar's test"}}{ }
#' \item{\code{"Chi-squared goodness-of-fit test"}}{ }
#' \item{\code{"Pearson's chi-squared test"}}{ }
#' \item{\code{"Exact McNemar's test"}}{ }
#' \item{\code{"Boschloo's test"}}{ }
#' \item{\code{"Wilcoxon's one-sample signed-rank test"}}{ }
#' \item{\code{"Mann-Whitney's U test"}}{ }
#' \item{\code{"Kruskal-Wallis's one-way ANOVA"}}{ }
#' \item{\code{"Student's paired t-test"}}{ }
#' \item{\code{"Mixed model ANOVA"}}{ }
#' \item{\code{"Student's one-sample t-test"}}{ }
#' \item{\code{"Welch's two-sample t-test"}}{ }
#' \item{\code{"F-test (ANOVA)"}}{ }
#'   }
#'  }
#' }
#'
#' @section Combining rows:
#' The \code{reshape_rows} argument offers a framework for combining multiple rows of the output table into a single one.
#' \code{reshape_rows} is a named list of lists. The names of it's member-lists determine the name that will be displayed
#' as the name of the combined summary stats in the table (e.g. "mean ± sd "). The member lists need to contain two
#' elements: \code{args}, contains the names of the summary statistics to be combined as characters, and
#' \code{fun} which contains a function to combine these summary stats. The argument names of this function need to match
#' the character strings specified in \code{args}. Check out the default options for an exemplary definition.
#'
#' @return
#' Returns a A \code{DescrList} object, which is a named list of descriptive statistics
#' which can be passed along to the print function to create
#' pretty summary tables.
#'
#' @references [1] Wilson, E. B. (1927). "Probable inference, the law of succession, and statistical inference". Journal of the American Statistical Association. 22 (158): 209-212. doi:10.1080/01621459.1927.10502953. JSTOR 2276774
#'
#'
#' [2] Hodges, J. L.; Lehmann, E. L. (1963). "Estimation of location based on ranks". Annals of Mathematical Statistics. 34 (2): 598-611. doi:10.1214/aoms/1177704172. JSTOR 2238406. MR 0152070. Zbl 0203.21105. PE euclid.aoms/1177704172
#'
#' @examples
#' descr(iris)
#' DescrList <- descr(iris)
#' DescrList$variables$results$Sepal.Length$Total$mean
#' print(DescrList)
#' descr(iris, "Species")
#' @export
#'
#' @import dplyr
#' @importFrom magrittr `%<>%`
#' @import tibble
#' @import forcats
#' @import stringr
#' @import scales
#'
descr <-
  function(dat,
           group = NULL,
           group_labels = list(),
           var_labels = list(),
           var_options = list(),
           summary_stats_cont = list(
             N = DescrTab2:::.N,
             Nmiss = DescrTab2:::.Nmiss,
             mean = DescrTab2:::.mean,
             sd = DescrTab2:::.sd,
             median = DescrTab2:::.median,
             Q1 = DescrTab2:::.Q1,
             Q3 = DescrTab2:::.Q3,
             min = DescrTab2:::.min,
             max = DescrTab2:::.max
           ),
           summary_stats_cat = list(),
           format_summary_stats = list(
             N = function(x) {
               format(x, digits = 2, scientific = 3)
             },
             Nmiss = function(x) {
               format(x, digits = 2, scientific = 3)
             },
             mean = function(x) {
               format(x, digits = 2, scientific = 3)
             },
             sd = function(x) {
               format(x, digits = 2, scientific = 3)
             },
             median = function(x) {
               format(x, digits = 2, scientific = 3)
             },
             Q1 = function(x) {
               format(x, digits = 2, scientific = 3)
             },
             Q3 = function(x) {
               format(x, digits = 2, scientific = 3)
             },
             min = function(x) {
               format(x, digits = 2, scientific = 3)
             },
             max = function(x) {
               format(x, digits = 2, scientific = 3)
             },
             CI = function(x) {
               format(x, digits = 2, scientific = 3)
             }
           ),
           format_p = scales::pvalue_format(),
           format_options = list(
             print_Total = NULL,
             print_p = TRUE,
             print_CI = TRUE,
             combine_mean_sd = FALSE,
             combine_median_Q1_Q3 = FALSE,
             omit_Nmiss_if_0 = TRUE,
             omit_missings_in_group = TRUE,
             percent_accuracy = NULL,
             percent_suffix = "%",
             row_percent = FALSE,
             Nmiss_row_percent = FALSE,
             absolute_relative_frequency_mode = c(
               "both",
               "only_absolute",
               "only_relative"
             ),
             omit_missings_in_categorical_var = FALSE,
             categorical_missing_percent_mode = c(
               "no_missing_percent",
               "missing_as_regular_category",
               "missing_as_separate_category"
             ),
             caption = NULL,
             replace_empty_string_with_NA = TRUE
           ),
           test_options = list(
             paired = FALSE,
             nonparametric = FALSE,
             exact = FALSE,
             indices = c(),
             guess_id = TRUE,
             include_group_missings_in_test = FALSE,
             include_categorical_missings_in_test = FALSE,
             test_override = NULL,
             additional_test_args = list()
           ),
           reshape_rows = list(
             `Q1 - Q3` = list(
               args = c("Q1", "Q3"),
               fun = function(Q1, Q3) {
                 paste0(Q1, " -- ", Q3)
               }
             ),
             `min - max` = list(
               args = c("min", "max"),
               fun = function(min, max) {
                 paste0(min, " -- ", max)
               }
             )
           ),
           ...) {


    # Coerce dataset to tibble
    dat %<>% as_tibble()
    # Extract labels from dataset if available
    var_labels %<>% as.list()
    extracted_labels <- extract_labels(dat)
    name_differences_labels <- setdiff(names(extracted_labels), names(var_labels))
    var_labels[name_differences_labels] <-
      extracted_labels[name_differences_labels]
    dat %<>% unlabel()

    # Coerce all date columns to factors
    if (isTRUE(any(sapply(dat, function(x) inherits(x, "Date"))))) {
      warning("Your dataset contains variables of type 'Date'. These are automatically converted to factors")
      dat %<>% mutate(across(where(function(x) inherits(x, "Date")), function(x) {
        x %>%
          as.factor() %>%
          fct_explicit_na()
      }))
    }
    # Format options have to be cleaned first, because the next data cleaning step depends on them
    format_options %<>% as.list()

    ### Is format_options a named list?
    if (length(format_options) > 0 &&
      is.null(names(format_options))) {
      warning("names(format_options) cannot be empty. Ignoring the format_options option.")
    }
    format_options <-
      fill_list_with_default_arguments(format_options, descr, "format_options")


    # Check for empty strings
    if (any(dat == "", na.rm = TRUE)) {
      if (isTRUE(format_options[["replace_empty_string_with_NA"]])) {
        dat %<>% mutate(across(everything(), ~ na_if(., "")))
        dat %<>% mutate(across(where(function(x) {
          "" %in% levels(x)
        }), ~ fct_drop(., c(""))))
      } else {
        if (any(dat == "(empty)", na.rm = TRUE)) {
          stop('Your data contains both "" (i.e. empty strings) and "(empty)". This would lead to conflicts. Rename your "" data. ')
        }
        warning('Your data contains "" (i.e., emtpy strings). If "" is used to code missings, you should
              recode it. DescrTab2 assumes "" is supposed to mean something, and converted it to "(empty)".')
        dat %<>% mutate(
          across(where(function(x) is.factor(x)), function(x) fct_recode(x, "(empty)" = "")),
          across(where(is.character), function(x) if_else(x == "", "(empty)", x))
        )
      }
    }


    test_options %<>% as.list()

    if (isTRUE(test_options[["guess_id"]])){
      test_options[["indices"]] <- guess_ID_variable(dat)
    }
    if (is.character(test_options[["indices"]]) &&
      length(test_options[["indices"]]) == 1 &&
      test_options[["indices"]] %in% names(dat)) {
      idx_name <- test_options[["indices"]]
      test_options[["indices"]] <- dat %>% pull(!!idx_name)
      dat %<>% select(-!!idx_name)

      if (isTRUE(test_options[["guess_id"]])){
        warning("ID variable candidate automatically extracted from the dataset.")
      }
    }



    ### Is group either null or a length one character or numeric?
    if (!is.null(group) &
      !((is.character(group) |
        is.numeric(group)) & length(group) == 1)) {
      stop(
        "group has to be either NULL or a character specifiying the name of the group variable,
        or a length one numeric specifying the column number of the group variable"
      )
    }

    # Remove group column from dataset & coerce group to factor
    if (!is.null(group)) {
      if (isTRUE(format_options[["omit_missings_in_group"]])) {
        if (dat %>% pull(!!group) %>% is.na() %>% any()) {
          warning(
            "Observations with missings in the group variable were dropped. To include them as a separate category, specify
            format_options = list(omit_missings_in_group=FALSE)"
          )
          dat %<>% filter(!is.na(get(!!group)))
        }
      }

      group_var <-
        dat %>%
        pull(!!group) %>%
        as_factor() %>%
        fct_explicit_na()
      dat %<>% select(-!!group)

      group_levels <- levels(group_var)
      names(group_levels) <- group_levels
    } else {
      group_var <- NULL
      group_levels <- NULL
    }
    var_names <- names(dat)
    names(var_names) <- var_names

    ## Check if input was specified correctly
    ### Is dat empty?
    if (nrow(dat) == 0 | ncol(dat) == 0) {
      stop("dat has 0 rows or 0 columns.")
    }

    if (!all(
      sapply(dat, function(x) {
        class(x)[[1]]
      }) %in% c(
        "numeric",
        "integer",
        "factor",
        "ordered",
        "character",
        "logical"
      )
    )) {
      stop("You may only have numeric, factor or character columns in your data.")
    }

    # Coerce all non-numeric columns to factors
    dat %<>% mutate(across(-where(is.numeric), function(x) {
      x %>%
        as_factor() %>%
        fct_explicit_na()
    }))


    # Input option cleaning (except format_options, which were cleaned in the beginning)
    ## If options lists were passed as named named vectors, coerce to list
    group_labels %<>% as.list()

    var_options <- lapply(var_options, as.list)


    ### Is group_labels a named list?
    if (length(group_labels) > 0 && is.null(names(group_labels))) {
      warning("names(group_labels) cannot be empty. Ignoring the group_labels option.")
    }
    ### Is test_options a named list?
    if (length(test_options) > 0 && is.null(names(test_options))) {
      warning("names(test_options) cannot be empty. Ignoring the test_options option.")
    }

    if (isTRUE(format_options[["omit_missings_in_categorical_var"]] == TRUE) & isTRUE(test_options[["include_group_missings_in_test"]] == TRUE)) {
      stop("You have format_options[['omit_missings_in_categorical_var']] == FALSEand test_options[['include_group_missings_in_test']] == TRUE,
           i.e. you request a statistical test that treats missings in categorical variables as regular observations, but you do not report the number of missings.
           You should avoid this.")
    }

    ### Is reshape_rows a named list?
    if (length(reshape_rows) > 0 && is.null(names(reshape_rows))) {
      stop("names(reshape_rows) may not be empty.")
    }

    ### Does var_labels contain labels for variables not in the dataset?
    if (!all(names(var_labels) %in% var_names)) {
      warning("Not all variables from var_labels are present in dat.")
    }
    ### Does var_options contain options for variables not in the dataset?
    if (!all(names(var_options) %in% var_names)) {
      warning("Not all variable names from var_options are present in dat.")
    }
    ### is summary_stats_cont a list of functions?
    if (!is.list(summary_stats_cont) |
      !all(sapply(summary_stats_cont, is.function))) {
      stop("summary_stats_cont must be a list of functions.")
    }
    ### is summary_stats_cat an empty list or a  list of functions?
    if ((length(summary_stats_cat) > 0) &
      (!is.list(summary_stats_cat) |
        !all(sapply(summary_stats_cat, is.function)))) {
      stop("summary_stats_cat must be a list of functions.")
    }
    ### Is summary_stats_cat a named list?
    if (length(summary_stats_cat) > 0 &&
      is.null(names(summary_stats_cat))) {
      warning(
        "names(summary_stats_cat) cannot be empty. Ignoring the summary_stats_cat option."
      )
    }
    ### is format_p a function?
    if (!is.function(format_p)) {
      stop("format_p must be a function.")
    }
    ### is format_summary_stats a list of functions?
    if (!is.list(format_summary_stats) |
      !all(sapply(format_summary_stats, is.function))) {
      stop("format_summary_stats must be a list of functions.")
    }
    ### Is format_summary_stats a named list?
    if (length(format_summary_stats) > 0 &&
      is.null(names(format_summary_stats))) {
      warning(
        "names(format_summary_stats) cannot be empty. Ignoring the format_summary_stats option."
      )
    }
    ### is reshape_rows a list of lists?
    if (!all(sapply(reshape_rows, is.list))) {
      stop("reshape_rows must be a list of lists")
    }

    format_summary_stats <-
      fill_list_with_default_arguments(format_summary_stats, descr, "format_summary_stats")
    ### do all summary_stats have a corresponding format function?
    tmp_names <- names(format_summary_stats)
    if (!all(names(c(summary_stats_cat, summary_stats_cont)) %in% tmp_names)) {
      warning(
        "All summary stats must have a corresponding formatting function. Defaulting to as.character"
      )
      format_summary_stats[setdiff(names(c(
        summary_stats_cat, summary_stats_cont
      )), tmp_names)] <-
        sapply(setdiff(names(
          c(summary_stats_cat, summary_stats_cont)
        ), tmp_names), function(x) {
          return(as.character)
        },
        USE.NAMES = TRUE,
        simplify = FALSE
        )
    }
    ## Fill incomplete input lists with default parameters
    if (!is.null(group)) {
      tmp_names <- setdiff(names(group_labels), group_levels)
      group_labels[tmp_names] <- group_levels[tmp_names]
    }

    tmp_names <- setdiff(var_names, names(var_labels))
    var_labels[tmp_names] <- var_names[tmp_names]
    for (var_name in var_names) {
      if (is.null(var_options[[var_name]][["label"]])) {
        if (!is.list(var_options[[var_name]])) {
          var_options[[var_name]] <- as.list(var_options[[var_name]])
        }
        var_options[[var_name]][["label"]] <- var_labels[[var_name]]
      }
    }
    test_options <-
      fill_list_with_default_arguments(test_options, descr, "test_options")

    if (isTRUE(test_options[["paired"]]) && is.null(format_options[["print_Total"]])) {
      message("You specified paired tests and did not explicitly
specify format_options$print_Total. print_Total is set to FALSE.")
      format_options[["print_Total"]] <- FALSE
    } else if (!isTRUE(test_options[["paired"]]) && is.null(format_options[["print_Total"]])) {
      format_options[["print_Total"]] <- TRUE
    }

    reshape_rows <-
      fill_list_with_default_arguments(reshape_rows, descr, "reshape_rows")

    if (isTRUE(format_options[["combine_mean_sd"]])) {
      reshape_rows[["mean \u00B1 sd"]] <- list(
        args = c("mean", "sd"),
        fun = function(mean, sd) {
          paste0(mean, " \u00B1 ", sd)
        }
      )
    }

    if (isTRUE(format_options[["combine_median_Q1_Q3"]])) {
      reshape_rows[["Q1 - Q3"]] <- NULL
      reshape_rows[["median (Q1, Q3)"]] <- list(
        args = c("median", "Q1", "Q3"),
        fun = function(median, Q1, Q3) {
          paste0(median, " (", Q1, ", ", Q3, ")")
        }
      )
    }


    for (var_option_name in names(var_options)) {
      if (!is.null(var_options[[var_option_name]][["summary_stats"]])) {
        if (!is.list(var_options[[var_option_name]][["summary_stats"]]) |
          !all(sapply(var_options[[var_option_name]][["summary_stats"]], is.function))) {
          stop("summary_stats in var_options must be a list of functions.")
        }
      }
      if (!is.null(var_options[[var_option_name]][["summary_stats"]]) ||
        !is.null(var_options[[var_option_name]][["format_summary_stats"]])) {
        name_diff <-
          setdiff(names(format_summary_stats), names(var_options[[var_option_name]][["format_summary_stats"]]))
        var_options[[var_option_name]][["format_summary_stats"]][name_diff] <-
          format_summary_stats[name_diff]

        tmp_names <-
          names(var_options[[var_option_name]][["format_summary_stats"]])
        if (!all(names(var_options[[var_option_name]][["summary_stats"]]) %in% tmp_names)) {
          warning(
            "All summary stats in var_options must have a corresponding formatting function. Defaulting to as.character"
          )
          var_options[[var_option_name]][["format_summary_stats"]][setdiff(names(var_options[[var_option_name]][["summary_stats"]]), tmp_names)] <-
            sapply(setdiff(names(var_options[[var_option_name]][["summary_stats"]]), tmp_names), function(x) {
              return(as.character)
            },
            USE.NAMES = TRUE,
            simplify = FALSE
            )
        }
      }
      if (!is.null(var_options[[var_option_name]][["format_p"]])) {
        if (!is.function(var_options[[var_option_name]][["format_p"]])) {
          stop("format_p in var_options must be a function.")
        }
      }
      if (!is.null(var_options[[var_option_name]][["format_options"]])) {
        name_diff <-
          setdiff(names(format_options), names(var_options[[var_option_name]][["format_options"]]))
        var_options[[var_option_name]][["format_options"]][name_diff] <-
          format_options[name_diff]
      }
      if (!is.null(var_options[[var_option_name]][["test_options"]])) {
        if (!is.null(var_options[[var_option_name]][["format_options"]])) {
          if (isTRUE(var_options[[var_option_name]][["format_options"]][["omit_missings_in_categorical_var"]] == TRUE) & isTRUE(var_options[[var_option_name]][["test_options"]][["include_group_missings_in_test"]] == TRUE)) {
            stop("You have format_options[['omit_missings_in_categorical_var']] == FALSEand test_options[['include_group_missings_in_test']] == TRUE,
           i.e. you request a statistical test that treats missings in categorical variables as regular observations, but you do not report the number of missings.
           You should avoid this.")
          }
        }
        name_diff <-
          setdiff(names(test_options), names(var_options[[var_option_name]][["test_options"]]))
        var_options[[var_option_name]][["test_options"]][name_diff] <-
          test_options[name_diff]
      }
      if (!is.null(var_options[[var_option_name]][["test_override"]])) {
        if (!(var_options[[var_option_name]][["test_override"]] %in% print_test_names())) {
          warning(paste0(
            "test_override has to be one of: ",
            paste(print_test_names(), collapse = ", ")
          ))
        }
      }
      if (!is.null(var_options[[var_option_name]][["reshape_rows"]])) {
        name_diff <-
          setdiff(names(reshape_rows), names(var_options[[var_option_name]][["reshape_rows"]]))
        var_options[[var_option_name]][["reshape_rows"]][name_diff] <-
          reshape_rows[name_diff]
      }
      if (isTRUE(var_options[[var_option_name]][["format_options"]][["combine_mean_sd"]])) {
        name_diff <-
          setdiff(names(reshape_rows), names(var_options[[var_option_name]][["reshape_rows"]]))
        var_options[[var_option_name]][["reshape_rows"]][name_diff] <-
          reshape_rows[name_diff]
        var_options[[var_option_name]][["reshape_rows"]][["mean \u00B1 sd"]] <-
          list(
            args = c("mean", "sd"),
            fun = function(mean, sd) {
              paste0(mean, " \u00B1 ", sd)
            }
          )
      }

      if (isTRUE(var_options[[var_option_name]][["format_options"]][["combine_median_Q1_Q3"]])) {
        name_diff <-
          setdiff(names(reshape_rows), names(var_options[[var_option_name]][["reshape_rows"]]))
        var_options[[var_option_name]][["reshape_rows"]][name_diff] <-
          reshape_rows[name_diff]
        var_options[[var_option_name]][["reshape_rows"]][["Q1 - Q3"]] <- NULL

        var_options[[var_option_name]][["reshape_rows"]][["median (Q1, Q3)"]] <-
          list(
            args = c("median", "Q1", "Q3"),
            fun = function(median, Q1, Q3) {
              paste0(median, " (", Q1, ", ", Q3, ")")
            }
          )
      }
    }


    # Create list where all results will be saved
    ergs <- list()
    ergs[["input_facts"]] <-
      list(nrow = nrow(dat), ncol = ncol(dat))
    ergs[["group"]] <- list()
    ergs[["group"]][["var"]] <- group_var
    ergs[["group"]][["name"]] <- group
    ergs[["group"]][["labels"]] <- group_labels
    ergs[["group"]][["levels"]] <- group_levels

    group_n <- c()
    for (lvl in levels(ergs[["group"]][["var"]])) {
      group_n <- c(group_n, sum(ergs[["group"]][["var"]] == lvl))
    }

    ## Reminder: Add option to exclude Missings
    if (is.null(ergs[["group"]][["var"]])) {
      group_n <- ergs[["input_facts"]][["nrow"]]
    } else {
      group_n <- c(group_n, sum(group_n))
    }
    names(group_n) <- c(group_levels, "Total")
    ergs[["group"]][["lengths"]] <- group_n


    ergs[["variables"]] <- list()

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
          test_options
        )
      } else if (is.numeric(var)) {
        # Analyze continuous variable
        var_descr <- descr_cont(var,
          group_var,
          var_name,
          summary_stats_cont,
          var_options = var_options[[var_name]],
          test_options
        )
      }
      # Append result of analysis to list
      ergs[["variables"]][[var_name]] <- var_descr
    }

    # Save formatting options for printing later
    ergs[["var_names"]] <- var_names
    ergs[["var_options"]] <- var_options
    ergs[["format"]] <- list()
    ergs[["format"]][["p"]] <- format_p
    ergs[["format"]][["summary_stats"]] <- format_summary_stats
    ergs[["format"]][["options"]] <- format_options
    ergs[["format"]][["reshape_rows"]] <- reshape_rows

    # Make result a "DescrList" object and return
    attr(ergs, "class") <- c("DescrList", "list")
    return(ergs)
  }


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
    # Test options choice: Special test options have precendence over global test options
    if (!is.null(var_options[["test_options"]])) {
      test_options <- var_options[["test_options"]]
    }
    # Was a specific test requested?
    test_override <- test_options[["test_override"]]
    if (!is.null(var_options[["test_override"]])) {
      test_override <- var_options[["test_override"]]
    }


    for (group_name in levels(group)) {
      # Subset values for the respective group
      var_grp <- var[which(group == group_name)]
      cat_list <- list()
      cat_list[["summary_stats"]] <- list()

      for (summary_stat_name in names(summary_stats)) {
        cat_list[["summary_stats"]][[summary_stat_name]] <-
          summary_stats[[summary_stat_name]](var_grp)
      }

      cat_list[["categories"]] <- list()
      for (cat_name in var_levels) {
        cat_list[["categories"]][[cat_name]] <- sum(var_grp == cat_name)
      }
      erg[["results"]][[group_name]] <- cat_list
    }

    # Caclulate summary for whole cohort
    cat_list <- list()
    cat_list[["summary_stats"]] <- list()

    for (summary_stat_name in names(summary_stats)) {
      cat_list[["summary_stats"]][[summary_stat_name]] <-
        summary_stats[[summary_stat_name]](var)
    }
    cat_list[["categories"]] <- list()
    for (cat_name in var_levels) {
      cat_list[["categories"]][[cat_name]] <- sum(var == cat_name)
    }
    erg[["results"]][["Total"]] <- cat_list

    # Calculate test
    test_list <-
      erg[["test_list"]] <-
      tryCatch(
        test_cat(var, group, test_options, test_override, var_name),
        error = function(cond) {
          message("Error converted to warning: ", cond)
          list(
            p = NA_real_,
            test_name = "Test errored, check console output.",
            CI_name = "",
            CI = c(NA_real_, NA_real_)
          )
        }
      )

    erg[["variable_name"]] <- var_name
    erg[["variable_levels"]] <- var_levels
    erg[["variable_options"]] <- var_options
    erg[["variable_lengths"]] <- calc_variable_lengths(var, group)

    attr(erg, "class") <- c("cat_summary", "list")
    erg
  }

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
    # Test options choice: Special test options have precendence over global test options
    if (!is.null(var_options[["test_options"]])) {
      test_options <- var_options[["test_options"]]
    }
    # Was a specific test requested?
    test_override <- test_options[["test_override"]]
    if (!is.null(var_options[["test_override"]])) {
      test_override <- var_options[["test_override"]]
    }


    for (group_name in levels(group)) {
      # Subset values for the respective group
      var_grp <- var[which(group == group_name)]
      group_list <- list()

      for (summary_stat_name in names(summary_stats)) {
        group_list[[summary_stat_name]] <-
          summary_stats[[summary_stat_name]](var_grp)
      }
      erg[["results"]][[group_name]] <- group_list
    }

    # Calculate summary for whole cohort
    tot_list <- list()

    for (summary_stat_name in names(summary_stats)) {
      tot_list[[summary_stat_name]] <-
        summary_stats[[summary_stat_name]](var)
    }
    erg[["results"]][["Total"]] <- tot_list

    # Calculate test
    erg[["test_list"]] <-
      tryCatch(
        test_cont(var, group, test_options, test_override, var_name),
        error = function(cond) {
          message("Error converted to warning: ", cond)
          list(
            p = NA_real_,
            test_name = "Test errored, check console output.",
            CI_name = "",
            CI = c(NA_real_, NA_real_)
          )
        }
      )
    erg[["variable_name"]] <- var_name
    erg[["variable_options"]] <- var_options
    erg[["variable_lengths"]] <- calc_variable_lengths(var, group)

    attr(erg, "class") <- c("cont_summary", "list")
    erg
  }

fill_list_with_default_arguments <-
  function(lst, fn, default_arg_name) {
    default_arg <- eval(formals(fn)[[default_arg_name]])
    name_difference <- setdiff(names(default_arg), names(lst))
    lst[name_difference] <- default_arg[name_difference]
    lst
  }


calc_variable_lengths <- function(var, group) {
  if (is.numeric(var)) {
    erg <- list()
    erg[["Total"]] <- list()
    erg[["Total"]][["N"]] <- length(var)
    erg[["Total"]][["Nmiss"]] <- sum(is.na(var))

    if (!is.null(group)) {
      for (group_name in levels(group)) {
        erg[[group_name]] <- list()
        erg[[group_name]][["N"]] <- length(var[group == group_name])
        erg[[group_name]][["Nmiss"]] <-
          sum(is.na(var[group == group_name]))
      }
    }
    return(erg)
  } else {
    erg <- list()
    erg[["Total"]] <- list()
    erg[["Total"]][["N"]] <- length(var)
    erg[["Total"]][["Nmiss"]] <- sum(var == "(Missing)")

    if (!is.null(group)) {
      for (group_name in levels(group)) {
        erg[[group_name]] <- list()
        erg[[group_name]][["N"]] <- length(var[group == group_name])
        erg[[group_name]][["Nmiss"]] <-
          sum(var[group == group_name] == "(Missing)")
      }
    }
    return(erg)
  }
}

#' S3 override for print function for DescrList objects.
#'
#' This function takes a DescrList object and converts it to either a DescrPrintCharacter or DescrPrintNumeric object,
#' depending on the print_format option. This object is then printed in an appropriate format.
#'
#' @details There is no way to convert between DescrPrintCharacter and DescrPrintNumeric objects. The first type is for
#' what you would usually want, the second type is mostly for debugging purposes. A DescrPrintCharacter object can
#' be printed as html, tex code, as a flextable object or simply to the console.
#'
#' @param x A \code{DescrList} object returned from \code{\link{descr}}.
#' @param print_format
#' Possible values: "console" (default), "tex", "html", "word", "numeric"
#' @param silent I TRUE, suppresses output to stdout.
#' @param ... further arguments to be passed along to print method
#'
#' @return
#' A DescrPrint object which can be printed in various formats.
#'
#' You can use the \code{print_format} option to control the output type. If you use 'DescrTab2' inside an .Rmd document,
#' you can set the clobal option \code{option(print_format="tex")} or \code{option(print_format="html")} or
#' \code{option(print_format="word")} depending on your document type. This way, all your tables will be printed in the
#' right format by default inside this document.
#'
#' @export
#'
#' @examples
#' print(descr(iris), print_format = "console")
#' print(descr(iris), print_format = "tex")
#' print(descr(iris), print_format = "html")
#' print(descr(iris), print_format = "word")
#' print(descr(iris), print_format = "numeric")
#' options(print_format = "tex")
#' descr(iris)
#' options(print_format = "console")
#' descr(iris)
#' DescrPrint <- print(descr(iris))
#' DescrPrint$variables$results$Sepal.Length$Total$mean
#' print(DescrPrint)
#' @import dplyr
#' @importFrom magrittr `%<>%`
#' @import tibble
#' @import forcats
#' @import stringr
print.DescrList <- function(x,
                            print_format = options("print_format")[[1]],
                            silent = FALSE,
                            ...) {
  DescrListObj <- x

  # if no printing format was set, print to console
  if (is.null(print_format)) {
    print_format <- "console"
  }

  # Preprocessing of the DescrListObj for printing.
  # In this step, formatting rules are applied.
  DescrPrintObj <- create_DescrPrint(DescrListObj, print_format)

  # Print the DescrPrint object
  print(DescrPrintObj,
    print_format = print_format,
    silent = silent,
    ...
  )
}


#' S3 override for print function for DescrPrint objects
#'
#' @keywords internal
#'
#' @param x A \code{DescrList} object returned from \code{\link{descr}}.
#' @param print_format
#' Possible values: "console" (default), "tex", "html", "word", "numeric"
#' @param silent I TRUE, suppresses output to stdout.
#' @param ... further arguments to be passed along to print method
#'
#' @return
#' A DescrPrint object which can be printed in various formats.
#'
#' @export
#'
#' @examples
#' descr(iris)
#' DescrPrint <- print(descr(iris))
#' DescrPrint$variables$results$Sepal.Length$Total$mean
#' print(DescrPrint)
#' @import dplyr
#' @importFrom magrittr `%<>%`
#' @import tibble
#' @import forcats
#' @import stringr
print.DescrPrint <- function(x,
                             print_format = options("print_format")[[1]],
                             silent = FALSE,
                             ...) {
  # if no printing format was set, print to console
  if (is.null(print_format)) {
    print_format <- "console"
  }
  NextMethod(
    "print",
    object = x,
    print_format = print_format,
    silent = silent,
    ...
  )
}

print.DescrPrintCharacter <- function(x,
                                      print_format = options("print_format")[[1]],
                                      silent = FALSE,
                                      ...) {
  ret <- switch(print_format,
    tex = print_tex(x, silent),
    html = print_html(x, silent),
    word = print_word(x, silent),
    print_console(x, silent, ...)
  )
  invisible(ret)
}

print.DescrPrintNumeric <- function(x,
                                    print_format = options("print_format")[[1]],
                                    silent = FALSE,
                                    ...) {
  ret <- print_numeric(x, silent, ...)
  invisible(ret)
}


create_DescrPrint <- function(DescrListObj, print_format) {
  create_subtable <- switch(print_format,
    numeric = create_numeric_subtable,
    create_character_subtable
  )

  var_names <- names(DescrListObj[["variables"]])
  group_names <- c(
    DescrListObj[["group"]][["var"]] %>% levels(),
    "Total"
  )


  format_summary_stats <-
    DescrListObj[["format"]][["summary_stats"]]
  format_p <- DescrListObj[["format"]][["p"]]
  format_options <- DescrListObj[["format"]][["options"]]
  reshape_rows <- DescrListObj[["format"]][["reshape_rows"]]

  print_list <- list()

  for (var_name in var_names) {
    var_option <-
      DescrListObj[["variables"]][[var_name]][["variable_options"]]

    if (!is.null(var_option[["format_options"]])) {
      var_format_options <- var_option[["format_options"]]
    } else {
      var_format_options <- format_options
    }

    if (!is.null(var_option[["format_summary_stats"]])) {
      var_format_summary_stats <- var_option[["format_summary_stats"]]
    } else {
      var_format_summary_stats <- format_summary_stats
    }

    if (!is.null(var_option[["format_p"]])) {
      var_format_p <- var_option[["format_p"]]
    } else {
      var_format_p <- format_p
    }
    if (!is.null(var_option[["reshape_rows"]])) {
      var_reshape_rows <- var_option[["reshape_rows"]]
    } else {
      var_reshape_rows <- reshape_rows
    }


    if (print_format == "numeric") {
      if ("cont_summary" %in% class(DescrListObj[["variables"]][[var_name]])) {
        if (!all(sapply(DescrListObj[["variables"]][[var_name]][["results"]][["Total"]], is.numeric))) {
          stop(
            "You can only create numeric tables if all of your summary statistics return numeric values."
          )
        }
      } else {
        if (!all(sapply(DescrListObj[["variables"]][[var_name]][["results"]][["Total"]][["summary_stats"]], is.numeric))) {
          stop(
            "You can only create numeric tables if all of your summary statistics return numeric values."
          )
        }
      }
    }

    print_list[[var_name]] <-
      DescrListObj[["variables"]][[var_name]] %>% create_subtable(
        .,
        var_name,
        var_format_options,
        var_format_summary_stats,
        var_format_p,
        var_reshape_rows
      )
  }

  printObj <- list()
  printObj[["variables"]] <- list()
  printObj[["lengths"]] <- list()
  printObj[["group"]] <- DescrListObj[["group"]]
  printObj[["caption"]] <- DescrListObj[["format"]][["options"]][["caption"]]
  printObj[["group_names"]] <- group_names


  group_labels <- c()
  for (name in group_names) {
    if (!is.null(DescrListObj[["group"]][["labels"]][[name]])) {
      group_labels <-
        c(group_labels, DescrListObj[["group"]][["labels"]][[name]])
    } else {
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

    tibl %<>% bind_rows(print_list[[var_name]][["tibble"]])
  }

  if (isTRUE(DescrListObj[["format"]][["options"]][["print_p"]] == FALSE)) {
    tibl %<>% select(-"p")
    tibl %<>% select(-"Test")
  }

  if (isTRUE(DescrListObj[["format"]][["options"]][["print_Total"]] == FALSE)) {
    tibl %<>% select(-"Total")
    group_labels <- group_labels[group_labels != "Total"]
  }

  if (isTRUE(DescrListObj[["format"]][["options"]][["print_CI"]] == FALSE)) {
    if (print_format == "numeric") {
      if ("CI_upper" %in% names(tibl) && "CI_lower" %in% names(tibl)) {
        tibl %<>% select(-c("CI_upper", "CI_lower"))
      }
    } else {
      if ("CI" %in% names(tibl)) {
        tibl %<>% select(-"CI")
      }
    }
  }

  names(tibl)[names(tibl) %in% group_names] <- group_labels
  printObj[["tibble"]] <- tibl

  attr(printObj, "class") <- switch(print_format,
    numeric = c("DescrPrint", "DescrPrintNumeric", "list"),
    c("DescrPrint", "DescrPrintCharacter", "list")
  )

  printObj
}



print_numeric <- function(DescrPrintObj,
                          silent = FALSE,
                          n = 1000,
                          width = NULL,
                          n_extra = NULL,
                          print_red_NA = FALSE) {
  tibl <- DescrPrintObj[["tibble"]]
  var_names <- names(DescrPrintObj[["variables"]])
  lengths <- c(unlist(DescrPrintObj[["lengths"]]) - 1)

  labels <- unlist(unlist(DescrPrintObj[["labels"]]))
  names(lengths) <- c(labels)

  indx_varnames <- logical()
  for (i in 1:length(DescrPrintObj$variables)) {
    indx_varnames <-
      c(indx_varnames, c(TRUE, rep(FALSE, DescrPrintObj$lengths[[i]] - 1)))
  }
  c1 <- tibl %>% pull(1)
  c1[!indx_varnames] <- paste0("  ", c1[!indx_varnames])
  tibl[, 1] <- c1

  print_format <- format(tibl,
    n = n,
    width = width,
    n_extra = n_extra
  ) %>% str_replace_all(pattern = fixed('"'), fixed(" "))

  if (!silent) {
    if (print_red_NA) {
      print_format %>% cli::cat_line()
    } else {
      print_format %>%
        str_replace_all(
          pattern = fixed("\033[31mNA\033[39m"),
          fixed("\033[31m  \033[39m")
        ) %>%
        cli::cat_line()
    }
  }
  invisible(DescrPrintObj)
}


print_console <- function(DescrPrintObj,
                          silent = FALSE,
                          n = 1000,
                          width = NULL,
                          n_extra = NULL,
                          print_red_NA = FALSE) {
  tibl <- DescrPrintObj[["tibble"]]
  var_names <- names(DescrPrintObj[["variables"]])
  lengths <- c(unlist(DescrPrintObj[["lengths"]]) - 1)

  labels <- unlist(unlist(DescrPrintObj[["labels"]]))
  names(lengths) <- c(labels)

  indx_varnames <- logical()
  for (i in 1:length(DescrPrintObj$variables)) {
    indx_varnames <-
      c(indx_varnames, c(TRUE, rep(FALSE, DescrPrintObj$lengths[[i]] - 1)))
  }
  c1 <- tibl %>% pull(1)
  c1[!indx_varnames] <- paste0("  ", c1[!indx_varnames])
  tibl[, 1] <- c1

  print_format <- format(tibl,
    n = n,
    width = width,
    n_extra = n_extra
  ) %>%
    .[-c(1, 3)] %>%
    str_replace_all(pattern = fixed('"'), fixed(" "))

  if (!silent) {
    cli::cat_line(print_format)
  }

  invisible(DescrPrintObj)
}

#' @importFrom kableExtra kbl kable_styling add_header_above column_spec
#' @importFrom utils capture.output head tail
print_tex <- function(DescrPrintObj, silent = FALSE) {
  tibl <- DescrPrintObj[["tibble"]]
  var_names <- names(DescrPrintObj[["variables"]])
  lengths <- c(unlist(DescrPrintObj[["lengths"]]) - 1)

  labels <- unlist(unlist(DescrPrintObj[["labels"]]))
  names(lengths) <- c(labels)
  caption <- DescrPrintObj[["caption"]]

  indx_varnames <- logical()
  for (i in 1:length(DescrPrintObj$variables)) {
    indx_varnames <-
      c(indx_varnames, c(TRUE, rep(FALSE, DescrPrintObj$lengths[[i]] - 1)))
  }

  # indx_varnames <- c1 %in% labels

  if ("p" %in% names(tibl)) {
    print_footnotes <- T
    tests <-
      tibl %>%
      filter(get("Test") != "") %>%
      pull("Test") %>%
      unique()
    p_vec <- tibl %>% pull("p")
    p_indx <- which(p_vec != "")
    test_abbrev <- create_test_abbreviations(tests)
    for (idx in p_indx) {
      tibl[idx, "p"] %<>% paste0("\\textsuperscript{", test_abbrev[match(tibl[idx, "Test"], tests)], "}")
    }
  } else {
    print_footnotes <- F
  }

  if ("Test" %in% names(tibl)) {
    tibl %<>% select(-"Test")
  }

  alig <- paste0(c("l", rep("c", ncol(tibl) - 1)), collapse = "")
  alig2 <- paste0(c("l", rep("c", ncol(tibl) - 1)))
  actual_colnames <- names(tibl[!indx_varnames, ])


  N_numbers <-
    c("", paste0("(N=", DescrPrintObj[["group"]][["lengths"]][names(DescrPrintObj[["group"]][["lengths"]]) %in% names(tibl)], ")"))
  pad_N <- ncol(tibl) - length(N_numbers)
  N_numbers <- c(N_numbers, rep("", pad_N))


  tibl <- escape_latex_symbols(tibl)

  width <- min(max(c(
    (sapply(labels, str_length) + 1) %/% 2,
    (sapply(tibl[[1]][!indx_varnames], str_length) + 1) %/% 2, 1
  )), 15)

  # For some reason, names need double escaping
  names(lengths) <- sapply(escape_latex_symbols(tibble(labels), doubleEscape = TRUE)[[1]],
    in_minpage,
    width = paste0(width + 1, "em"),
    doubleEscape = TRUE,
    strechSpace = FALSE
  )
  tibl[!indx_varnames, 1] <- sapply(tibl[[1]][!indx_varnames], in_minpage, width = paste0(width, "em"))

  tex <- tibl[!indx_varnames, ] %>%
    kbl(
      format = "latex",
      longtable = TRUE,
      booktabs = TRUE,
      linesep = "",
      align = alig,
      escape = FALSE,
      col.names = N_numbers,
      caption = caption
    ) %>%
    `if`(
      print_footnotes,
      kableExtra::footnote(., symbol = c(tests), symbol_manual = test_abbrev),
      .
    ) %>%
    column_spec(1, width = paste0(width + 1, "em")) %>%
    kableExtra::pack_rows(index = lengths, latex_gap_space = "0.5cm", escape = FALSE) %>%
    add_header_above(actual_colnames, line = FALSE, align = alig2) %>%
    kable_styling(
      latex_options = "repeat_header",
      repeat_header_continued = FALSE
    ) %>%
    capture.output()

  tex %<>% str_replace_all("\\\\\\\\(?!\\*)", fixed("\\\\\\\\*"))
  pagebreak_indices <-
    str_detect(tex, fixed("textbf")) %>%
    which() %>%
    tail(-1)
  if (length(head(pagebreak_indices, -1)) > 0) {
    tex[head(pagebreak_indices, -1) - 2] %<>% str_replace_all(
      fixed("\\\\*"),
      fixed("\\\\ \\noalign{\\vskip 0pt plus 12pt}")
    )
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
  if (!silent) {
    cli::cat_line(tex)
  }

  DescrPrintObj[["tex"]] <- tex

  invisible(DescrPrintObj)
}

#' @importFrom kableExtra kbl kable_styling add_header_above
#' @importFrom utils capture.output head tail
print_html <- function(DescrPrintObj, silent = FALSE) {
  tibl <- DescrPrintObj[["tibble"]]
  var_names <- names(DescrPrintObj[["variables"]])
  lengths <- c(unlist(DescrPrintObj[["lengths"]]) - 1)


  labels <- unlist(unlist(DescrPrintObj[["labels"]]))
  caption <- DescrPrintObj[["caption"]]
  names(lengths) <- c(labels)

  indx_varnames <- logical()
  for (i in 1:length(DescrPrintObj$variables)) {
    indx_varnames <-
      c(indx_varnames, c(TRUE, rep(FALSE, DescrPrintObj$lengths[[i]] - 1)))
  }

  if ("p" %in% names(tibl)) {
    print_footnotes <- T
    tests <-
      tibl %>%
      filter(get("Test") != "") %>%
      pull("Test") %>%
      unique()
    p_vec <- tibl %>% pull("p")
    p_indx <- which(p_vec != "")
    test_abbrev <- create_test_abbreviations(tests)
    for (idx in p_indx) {
      tibl[idx, "p"] %<>% paste0("<sup>", test_abbrev[match(tibl[idx, "Test"], tests)], "</sup>")
    }
  } else {
    print_footnotes <- F
  }

  if ("Test" %in% names(tibl)) {
    tibl %<>% select(-"Test")
  }

  alig <- paste0(c("l", rep("c", ncol(tibl) - 1)), collapse = "")
  alig2 <- paste0(c("l", rep("c", ncol(tibl) - 1)))
  actual_colnames <- names(tibl[!indx_varnames, ])
  N_numbers <-
    c("", paste0("(N=", DescrPrintObj[["group"]][["lengths"]][names(DescrPrintObj[["group"]][["lengths"]]) %in% names(tibl)], ")"))
  pad_N <- ncol(tibl) - length(N_numbers)
  N_numbers <- c(N_numbers, rep("", pad_N))


  html <- tibl[!indx_varnames, ] %>%
    kbl(
      format = "html",
      longtable = TRUE,
      booktabs = TRUE,
      linesep = "",
      align = alig,
      escape = FALSE,
      col.names = N_numbers,
      caption = caption
    ) %>%
    kable_styling() %>%
    `if`(
      print_footnotes,
      kableExtra::footnote(., symbol = c(tests), symbol_manual = test_abbrev),
      .
    ) %>%
    column_spec(1, width = "4.2cm") %>%
    kableExtra::pack_rows(index = lengths) %>%
    add_header_above(actual_colnames, line = FALSE, align = alig2)

  if (!silent) {
    cli::cat_line(html)
  }

  DescrPrintObj[["html"]] <- html

  invisible(DescrPrintObj)
}

#' @importFrom flextable flextable bold padding add_header border_inner align autofit
#' @importFrom officer fp_border
#' @importFrom utils capture.output head tail
print_word <- function(DescrPrintObj, silent = FALSE) {
  tibl <- DescrPrintObj[["tibble"]]
  caption <- DescrPrintObj[["caption"]]
  var_names <- names(DescrPrintObj[["variables"]])
  lengths <- c(unlist(DescrPrintObj[["lengths"]]) - 1)
  names(lengths) <- c(var_names)

  c1 <- tibl %>% pull(1)
  indx_varnames <- c1 %in% var_names

  if ("p" %in% names(tibl)) {
    print_footnotes <- T
    tests <-
      tibl %>%
      filter(get("Test") != "") %>%
      pull("Test") %>%
      unique()
    p_vec <- tibl %>% pull("p")
    p_indx <- which(p_vec != "")
    test_abbrev <- create_test_abbreviations(tests)
  } else {
    print_footnotes <- F
  }

  if ("Test" %in% names(tibl)) {
    tibl2 <- tibl %>% select(-"Test")
  } else {
    tibl2 <- tibl
  }


  actual_colnames <- DescrPrintObj[["group_names"]]
  N_numbers <-
    c(paste0("(N=", DescrPrintObj[["group"]][["lengths"]][names(DescrPrintObj[["group"]][["lengths"]]) %in% names(tibl)], ")"))
  names(N_numbers) <- unlist(DescrPrintObj[["group_labels"]][names(DescrPrintObj[["group"]][["lengths"]]) %in% names(tibl)])

  ft <- tibl2 %>%
    flextable() %>%
    bold(i = indx_varnames, j = 1) %>%
    padding(
      j = 1,
      i = !indx_varnames,
      padding.left = 20
    ) %>%
    add_header(top = FALSE, values = N_numbers) %>%
    flextable::border_inner(part = "header", border = officer::fp_border(width = 0)) %>%
    flextable::hline_bottom(part = "header", border = officer::fp_border(width = 2)) %>%
    align(
      j = which(names(tibl2) != "Variables"),
      part = "all",
      align = "center"
    ) %>%
    align(
      j = 1,
      part = "all",
      align = "left"
    )

  if (print_footnotes) {
    for (test in tests) {
      ft %<>% flextable::footnote(
        i = which((tibl %>% pull("Test")) %in% test),
        j = which(names(tibl2) == "p"),
        value = flextable::as_paragraph(c(test)),
        ref_symbols = c(test_abbrev[match(test, tests)]),
        part = "body"
      )
    }
  }
  if (!is.null(caption)) {
    ft <- flextable::set_caption(ft, caption)
  }


  ft <- ft %>%
    autofit()

  DescrPrintObj[["ft"]] <- ft

  if (!silent) {
    return(ft)
  } else {
    return(DescrPrintObj)
  }
}


#' S3 override for knit_print function for DescrList objects.
#'
#' @param x a
#' @param print_format b
#' @param silent c
#' @param ... abc
#' @return outputs formatted table depending on the environment (.RMD) which it is called from
#' @export
#'
knit_print.DescrList <-
  function(x,
           print_format = options("print_format")[[1]],
           silent = FALSE,
           ...) {
    DescrListObj <- x

    # if no printing format was set, print to console
    if (is.null(print_format)) {
      print_format <- "console"
    }

    # Preprocessing of the DescrListObj for printing.
    # In this step, formatting rules are applied.
    DescrPrintObj <- create_DescrPrint(DescrListObj, print_format)

    # Print the DescrPrint object
    knit_print(DescrPrintObj,
      print_format = print_format,
      silent = silent,
      ...
    )
  }


#' S3 override for knit_print function for DescrPrint objects.
#'
#' @param x a
#' @param print_format b
#' @param silent c
#' @param ... abc
#' @return outputs formatted table depending on the environment (.RMD) which it is called from
#' @export
#'
#' @importFrom knitr knit_print asis_output opts_knit opts_current fig_path
#' @importFrom rmarkdown pandoc_version
#' @importFrom flextable flextable_to_rmd
knit_print.DescrPrint <- function(x,
                                  print_format = print_format,
                                  silent = silent,
                                  ...) {
  if (knitr::is_html_output()) {
    str <- print.DescrPrintCharacter(x,
      print_format = "html",
      silent = TRUE
    )[["html"]]
    knit_print(asis_output(str))
  } else if (knitr::is_latex_output()) {
    str <- print.DescrPrintCharacter(x,
      print_format = "tex",
      silent = TRUE
    )[["tex"]]
    knit_print(asis_output(str))
  } else if (knitr::pandoc_to("docx")) {
    ft <- print.DescrPrintCharacter(x,
      print_format = "word",
      silent = TRUE
    )[["ft"]]
    is_bookdown <- isTRUE(opts_knit$get("bookdown.internal.label"))
    pandoc2 <- pandoc_version() >= numeric_version("2.0")
    str <- flextable_to_rmd(ft, bookdown = is_bookdown, pandoc2 = pandoc2, print = FALSE)
    knit_print(asis_output(str))
  } else {
    print(x, ...)
  }
}



create_numeric_subtable <- function(DescrVarObj,
                                    var_name,
                                    format_options,
                                    format_summary_stats,
                                    format_p,
                                    reshape_rows) {
  UseMethod("create_numeric_subtable")
}



create_numeric_subtable.cat_summary <-
  function(DescrVarObj,
           var_name,
           format_options,
           format_summary_stats,
           format_p,
           reshape_rows) {
    cat_names <- DescrVarObj[["variable_levels"]]
    summary_stat_names <-
      names(DescrVarObj[["results"]][["Total"]][["summary_stats"]])

    all_names <- c(summary_stat_names, cat_names)

    # DescrVarObj[["results"]][["Total"]][["categories"]][sapply(DescrVarObj[["Total"]], is.null)] <-
    #   NA

    tot <-
      c(
        NA_real_,
        unlist(DescrVarObj[["results"]][["Total"]][["summary_stats"]]),
        unlist(DescrVarObj[["results"]][["Total"]][["categories"]])
      )

    label <- DescrVarObj[["variable_options"]][["label"]]
    DescrVarObj[["label"]] <- label

    tibl <- tibble(Variable = c(
      label,
      all_names
    ))

    length_tibl <- nrow(tibl)
    groups <- setdiff(names(DescrVarObj[["results"]]), "Total")

    for (group in groups) {
      # DescrVarObj[[group]][sapply(DescrVarObj[[group]], is.null)] <- NA
      tmp <- c(
        NA_real_,
        unlist(DescrVarObj[["results"]][[group]][["summary_stats"]]),
        unlist(DescrVarObj[["results"]][[group]][["categories"]])
      )
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
      } else {
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

create_numeric_subtable.cont_summary <-
  function(DescrVarObj,
           var_name,
           format_options,
           format_summary_stats,
           format_p,
           reshape_rows) {
    summary_stat_names <- names(DescrVarObj[["results"]][["Total"]])

    # DescrVarObj[["Total"]][sapply(DescrVarObj[["Total"]], is.null)] <-
    #   NA
    tot <-
      c(NA_real_, unlist(DescrVarObj[["results"]][["Total"]]))


    label <- DescrVarObj[["variable_options"]][["label"]]
    DescrVarObj[["label"]] <- label

    tibl <- tibble(Variable = c(
      label,
      summary_stat_names
    ))

    length_tibl <- nrow(tibl)
    groups <- setdiff(names(DescrVarObj[["results"]]), "Total")

    for (group in groups) {
      # DescrVarObj[["results"]][[group]][sapply(DescrVarObj[["results"]][[group]], is.null)] <- NA
      tmp <-
        c(NA_real_, unlist(DescrVarObj[["results"]][[group]]))
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


create_character_subtable <- function(DescrVarObj,
                                      var_name,
                                      format_options,
                                      format_summary_stats,
                                      format_p,
                                      reshape_rows) {
  UseMethod("create_character_subtable")
}


create_character_subtable.cont_summary <-
  function(DescrVarObj,
           var_name,
           format_options,
           format_summary_stats,
           format_p,
           reshape_rows) {
    DescrVarObj_unformatted <- DescrVarObj
    groups <- setdiff(names(DescrVarObj[["results"]]), "Total")

    if (format_options[["omit_Nmiss_if_0"]] == TRUE) {
      if (isTRUE(DescrVarObj[["results"]][["Total"]][["Nmiss"]] == 0)) {
        DescrVarObj[["results"]][["Total"]] <-
          DescrVarObj[["results"]][["Total"]][setdiff(names(DescrVarObj[["results"]][["Total"]]), "Nmiss")]

        for (group in groups) {
          DescrVarObj[["results"]][[group]] <-
            DescrVarObj[["results"]][[group]][setdiff(names(DescrVarObj[["results"]][[group]]), "Nmiss")]
        }
      }
    }

    label <- DescrVarObj[["variable_options"]][["label"]]
    DescrVarObj[["label"]] <- label

    if (DescrVarObj[["variable_lengths"]][["Total"]][["Nmiss"]] == DescrVarObj[["variable_lengths"]][["Total"]][["N"]]) {
      all_summary_stats_missing <- T
    } else {
      all_summary_stats_missing <- F
    }

    if (all_summary_stats_missing) {
      length_tibl <- 2
      tibl <- bind_cols(Variables = c(label, "-"))

      for (group in groups) {
        tibl %<>% bind_cols(!!group := c("", "-"))
      }

      tibl %<>% bind_cols(Total = c("", "All entries NA"))
      tibl %<>% bind_cols(p = c("", "-"))
      tibl %<>% bind_cols(Test = c("", "-"))

      if (length(groups) == 2) {
        tibl %<>% bind_cols(CI = c("", "-"))
      }
    } else {
      summary_stat_names <- names(DescrVarObj[["results"]][["Total"]])


      if (!is.null(DescrVarObj[["results"]][["Total"]][["Nmiss"]])) {
        DescrVarObj[["results"]][["Total"]][["Nmiss"]] <-
          format_freqs(
            DescrVarObj[["results"]][["Total"]][["Nmiss"]],
            ifelse(format_options[["Nmiss_row_percent"]] == TRUE,
              DescrVarObj_unformatted[["results"]][["Total"]][["Nmiss"]],
              DescrVarObj_unformatted[["variable_lengths"]][["Total"]][["N"]]
            ),
            format_options[["absolute_relative_frequency_mode"]],
            format_options[["percent_accuracy"]],
            format_options[["percent_suffix"]]
          )
      }

      for (summary_stat in setdiff(summary_stat_names, "Nmiss")) {
        DescrVarObj[["results"]][["Total"]][[summary_stat]] <-
          format_summary_stats[[summary_stat]](DescrVarObj[["results"]][["Total"]][[summary_stat]])
      }

      for (reshape_name in names(reshape_rows)) {
        reshape <- reshape_rows[[reshape_name]]

        if (all(reshape[["args"]] %in% summary_stat_names)) {
          DescrVarObj[["results"]][["Total"]][[reshape[["args"]][[1]]]] <-
            do.call(reshape[["fun"]], DescrVarObj[["results"]][["Total"]][reshape[["args"]]])

          DescrVarObj[["results"]][["Total"]][reshape[["args"]][-1]] <-
            NULL
          name_indx <-
            which(names(DescrVarObj[["results"]][["Total"]]) == reshape[["args"]][[1]])
          names(DescrVarObj[["results"]][["Total"]])[name_indx] <-
            reshape_name
        }
      }

      summary_stat_names_pre_modification <-
        names(DescrVarObj[["results"]][["Total"]])

      tot <- DescrVarObj[["results"]][["Total"]]

      display_names <-
        names(DescrVarObj[["results"]][["Total"]]) %>% c(label, .)


      tibl <- bind_cols(Variables = display_names)
      length_tibl <- length(display_names)


      for (group in groups) {
        # DescrVarObj[["results"]][[group]][sapply(DescrVarObj[["results"]][[group]], is.null)] <- NA



        if (!is.null(DescrVarObj[["results"]][[group]][["Nmiss"]])) {
          DescrVarObj[["results"]][[group]][["Nmiss"]] <-
            format_freqs(
              DescrVarObj[["results"]][[group]][["Nmiss"]],
              ifelse(format_options[["Nmiss_row_percent"]] == TRUE,
                DescrVarObj_unformatted[["results"]][["Total"]][["Nmiss"]],
                DescrVarObj_unformatted[["variable_lengths"]][[group]][["N"]]
              ),
              format_options[["absolute_relative_frequency_mode"]],
              format_options[["percent_accuracy"]],
              format_options[["percent_suffix"]]
            )
        }

        for (summary_stat in setdiff(summary_stat_names, "Nmiss")) {
          DescrVarObj[["results"]][[group]][[summary_stat]] <-
            format_summary_stats[[summary_stat]](DescrVarObj[["results"]][[group]][[summary_stat]])
        }


        for (reshape_name in names(reshape_rows)) {
          reshape <- reshape_rows[[reshape_name]]

          if (all(reshape[["args"]] %in% summary_stat_names)) {
            DescrVarObj[["results"]][[group]][[reshape[["args"]][[1]]]] <-
              do.call(reshape[["fun"]], DescrVarObj[["results"]][[group]][reshape[["args"]]])

            DescrVarObj[["results"]][[group]][reshape[["args"]][-1]] <-
              NULL
            name_indx <-
              which(names(DescrVarObj[["results"]][[group]]) == reshape[["args"]][[1]])
            names(DescrVarObj[["results"]][[group]])[name_indx] <-
              reshape_name
          }
        }


        tibl %<>% bind_cols(!!group := c("", unlist(DescrVarObj[["results"]][[group]])))
      }

      tibl %<>% bind_cols(Total = c("", unlist(tot)))

      ## TODO: implement this
      # if(length_tibl>2){
      #      p_col <- rep("", length_tibl)
      # } else if(length_tibl==1){
      #
      # } else{
      #
      # }

      tibl %<>% bind_cols(p = c(
        "",
        if (isFALSE(format_options[["print_p"]])) {
          ""
        } else {
          format_p(DescrVarObj[["test_list"]]$p)
        },
        rep("", length_tibl -
          2)
      ))
      tibl %<>% bind_cols(Test = c(
        "",
        DescrVarObj[["test_list"]]$test_name,
        rep("", length_tibl -
          2)
      ))

      if (length(groups) == 2) {
        if (is.null(DescrVarObj[["test_list"]][["CI"]])) {
          CI_name <- ""
          CI <- ""
        } else {
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
        tibl %<>% bind_cols(CI = c(
          "",
          CI_name,
          CI,
          rep("", length_tibl -
            3)
        ))
      }
    }

    return(list(
      summary_list = DescrVarObj,
      length = length_tibl,
      tibble = tibl
    ))
  }

#' @import rlang
create_character_subtable.cat_summary <-
  function(DescrVarObj,
           var_name,
           format_options,
           format_summary_stats,
           format_p,
           reshape_rows) {
    ## Remember: Category levels may not be names "N"
    DescrVarObj_unformatted <- DescrVarObj

    cat_names <- DescrVarObj[["variable_levels"]]
    cat_names_nonmissing <- setdiff(cat_names, "(Missing)")
    summary_stat_names <-
      names(DescrVarObj[["results"]][["Total"]][["summary_stats"]])

    DescrVarObj[["results"]][["Total"]][["categories"]][sapply(DescrVarObj[["results"]][["Total"]][["categories"]], is.null)] <-
      "0 (0%)"

    N_total <-
      sum(unlist(DescrVarObj[["results"]][["Total"]][["categories"]][cat_names]))

    if (isTRUE(format_options[["categorical_missing_percent_mode"]][1] == "missing_as_regular_category") &
      format_options[["omit_missings_in_categorical_var"]] == FALSE) {
      N_nonmissing <- N_total
    } else {
      N_nonmissing <-
        sum(unlist(DescrVarObj[["results"]][["Total"]][["categories"]][cat_names_nonmissing]))
    }


    for (summary_stat in summary_stat_names) {
      DescrVarObj[["results"]][["Total"]][["summary_stats"]][[summary_stat]] <-
        format_summary_stats[[summary_stat]](DescrVarObj[["results"]][["Total"]][["summary_stats"]][[summary_stat]])
    }

    for (reshape_name in names(reshape_rows)) {
      reshape <- reshape_rows[[reshape_name]]

      if (all(reshape[["args"]] %in% summary_stat_names)) {
        DescrVarObj[["results"]][["Total"]][["summary_stats"]][[reshape[["args"]][[1]]]] <-
          do.call(reshape[["fun"]], DescrVarObj[["results"]][["Total"]][["summary_stats"]][reshape[["args"]]])

        DescrVarObj[["results"]][["Total"]][["summary_stats"]][reshape[["args"]][-1]] <-
          NULL
        name_indx <-
          which(names(DescrVarObj[["results"]][["Total"]][["summary_stats"]]) == reshape[["args"]][[1]])
        names(DescrVarObj[["results"]][["Total"]][["summary_stats"]])[name_indx] <-
          reshape_name
      }
    }

    for (cat_name in cat_names_nonmissing) {
      DescrVarObj[["results"]][["Total"]][["categories"]][[cat_name]] <-
        format_freqs(
          DescrVarObj[["results"]][["Total"]][["categories"]][[cat_name]],
          ifelse(format_options[["row_percent"]] == TRUE,
            DescrVarObj_unformatted[["results"]][["Total"]][["categories"]][[cat_name]],
            N_nonmissing
          ),
          format_options[["absolute_relative_frequency_mode"]],
          format_options[["percent_accuracy"]],
          format_options[["percent_suffix"]]
        )
    }

    if ("(Missing)" %in% cat_names) {
      if (format_options[["omit_missings_in_categorical_var"]] == TRUE) {
        DescrVarObj[["results"]][["Total"]][["categories"]][["(Missing)"]] <- NULL
      } else if (format_options[["categorical_missing_percent_mode"]][1] == "no_missing_percent") {
        DescrVarObj[["results"]][["Total"]][["categories"]][["(Missing)"]] <-
          as.character(DescrVarObj[["results"]][["Total"]][["categories"]][["(Missing)"]])
      } else {
        DescrVarObj[["results"]][["Total"]][["categories"]][["(Missing)"]] <-
          format_freqs(
            DescrVarObj[["results"]][["Total"]][["categories"]][["(Missing)"]],
            ifelse(format_options[["Nmiss_row_percent"]] == TRUE,
              DescrVarObj_unformatted[["results"]][["Total"]][["categories"]][["(Missing)"]],
              N_total
            ),
            format_options[["absolute_relative_frequency_mode"]],
            format_options[["percent_accuracy"]],
            format_options[["percent_suffix"]]
          )
      }
    }

    tot <- c(
      "",
      unlist(DescrVarObj[["results"]][["Total"]][["summary_stats"]]),
      unlist(DescrVarObj[["results"]][["Total"]][["categories"]])
    )


    label <- DescrVarObj[["variable_options"]][["label"]]
    DescrVarObj[["label"]] <- label

    tibl <- tibble(Variables = c(
      label,
      names(DescrVarObj[["results"]][["Total"]][["summary_stats"]]),
      cat_names[cat_names != "(Missing)" | !isTRUE(format_options[["omit_missings_in_categorical_var"]])]
    ))

    length_tibl <- nrow(tibl)
    groups <- setdiff(names(DescrVarObj[["results"]]), "Total")

    for (group in groups) {
      DescrVarObj[["results"]][[group]][["categories"]][sapply(DescrVarObj[["results"]][[group]][["categories"]], is.null)] <-
        "0 (0%)"


      N_group_total <-
        sum(unlist(DescrVarObj[["results"]][[group]][["categories"]][cat_names]))

      if (isTRUE(format_options[["categorical_missing_percent_mode"]][1] == "missing_as_regular_category") &
        format_options[["omit_missings_in_categorical_var"]] == FALSE) {
        N_group_nonmissing <- N_group_total
      } else {
        N_group_nonmissing <-
          sum(unlist(DescrVarObj[["results"]][[group]][["categories"]][cat_names_nonmissing]))
      }


      for (summary_stat in summary_stat_names) {
        DescrVarObj[["results"]][[group]][["summary_stats"]][[summary_stat]] <-
          format_summary_stats[[summary_stat]](DescrVarObj[["results"]][[group]][["summary_stats"]][[summary_stat]])
      }

      for (reshape_name in names(reshape_rows)) {
        reshape <- reshape_rows[[reshape_name]]

        if (all(reshape[["args"]] %in% summary_stat_names)) {
          DescrVarObj[["results"]][[group]][["summary_stats"]][[reshape[["args"]][[1]]]] <-
            do.call(reshape[["fun"]], DescrVarObj[["results"]][[group]][["summary_stats"]][reshape[["args"]]])

          DescrVarObj[["results"]][[group]][["summary_stats"]][reshape[["args"]][-1]] <-
            NULL
          name_indx <-
            which(names(DescrVarObj[["results"]][[group]][["summary_stats"]]) == reshape[["args"]][[1]])
          names(DescrVarObj[["results"]][[group]][["summary_stats"]])[name_indx] <-
            reshape_name
        }
      }


      for (cat_name in cat_names_nonmissing) {
        DescrVarObj[["results"]][[group]][["categories"]][[cat_name]] <-
          format_freqs(
            DescrVarObj[["results"]][[group]][["categories"]][[cat_name]],
            ifelse(format_options[["row_percent"]] == TRUE,
              DescrVarObj_unformatted[["results"]][["Total"]][["categories"]][[cat_name]],
              N_group_nonmissing
            ),
            format_options[["absolute_relative_frequency_mode"]],
            format_options[["percent_accuracy"]],
            format_options[["percent_suffix"]]
          )
      }

      if ("(Missing)" %in% cat_names) {
        if (format_options[["omit_missings_in_categorical_var"]] == TRUE) {
          DescrVarObj[["results"]][[group]][["categories"]][["(Missing)"]] <- NULL
        } else if (format_options[["categorical_missing_percent_mode"]][1] == "no_missing_percent") {
          DescrVarObj[["results"]][[group]][["categories"]][["(Missing)"]] <-
            as.character(DescrVarObj[["results"]][[group]][["categories"]][["(Missing)"]])
        } else {
          DescrVarObj[["results"]][[group]][["categories"]][["(Missing)"]] <-
            format_freqs(
              DescrVarObj[["results"]][[group]][["categories"]][["(Missing)"]],
              ifelse(format_options[["Nmiss_row_percent"]] == TRUE,
                DescrVarObj_unformatted[["results"]][["Total"]][["categories"]][["(Missing)"]],
                N_group_total
              ),
              format_options[["absolute_relative_frequency_mode"]],
              format_options[["percent_accuracy"]],
              format_options[["percent_suffix"]]
            )
        }
      }

      ## TODO: implement categories first option
      tmp <- c(
        "",
        unlist(DescrVarObj[["results"]][[group]][["summary_stats"]]),
        unlist(DescrVarObj[["results"]][[group]][["categories"]])
      )

      tibl %<>% bind_cols(!!group := tmp)
    }
    tibl %<>% bind_cols(Total = tot)

    tibl %<>% bind_cols(p = c(
      "",
      if (isFALSE(format_options[["print_p"]])) {
        ""
      } else {
        format_p(DescrVarObj[["test_list"]]$p)
      },
      rep("", length_tibl -
        2)
    ))
    tibl %<>% bind_cols(Test = c(
      "",
      DescrVarObj[["test_list"]]$test_name,
      rep("", length_tibl -
        2)
    ))


    if (length(groups) == 2) {
      if (is.null(DescrVarObj[["test_list"]][["CI"]])) {
        CI_name <- ""
        CI <- ""
      } else {
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
      tibl %<>% bind_cols(CI = c(
        "",
        CI_name,
        CI,
        rep("", length_tibl -
          3)
      ))
    }
    return(list(
      summary_list = DescrVarObj,
      length = length_tibl,
      tibble = tibl
    ))
  }



create_test_abbreviations <- function(test_names) {
  erg <- character()
  for (test in test_names) {
    abbrev <- switch(test,
      `Cochran's Q test` = "CocQ",
      `McNemar's test` = "McN",
      `Chi-squared goodness-of-fit test` = "chi1",
      `Pearson's chi-squared test` = "chi2",
      `Exact McNemar's test` = "eMcN",
      `Boschloo's test` = "Bolo",
      `Friedman test` = "Frie",
      `Wilcoxon two-sample signed-rank test` = "Wil2",
      `Wilcoxon's one-sample signed-rank test` = "Wil1",
      `Mann-Whitney's U test` = "MWU",
      `Kruskal-Wallis's one-way ANOVA` = "KW",
      `Student's paired t-test` = "tpar",
      `Mixed model ANOVA` = "MiAn",
      `Student's one-sample t-test` = "tt1",
      `Welch's two-sample t-test` = "tt2",
      `F-test (ANOVA)` = "F",
      "Unknown test"
    )

    erg <- c(erg, abbrev)
  }
  erg
}

#' Prints all possible tests names
#'
#' @return
#' Returns the names of all possible test names you can specify.
#' @export
#'
#' @examples
#' print_test_names()
print_test_names <- function() {
  c(
    "Cochran's Q test",
    "McNemar's test",
    "Chi-squared goodness-of-fit test",
    "Pearson's chi-squared test",
    "Exact McNemar's test",
    "Boschloo's test",
    "Friedman test",
    "Wilcoxon two-sample signed-rank test",
    "Wilcoxon's one-sample signed-rank test",
    "Mann-Whitney's U test",
    "Kruskal-Wallis's one-way ANOVA",
    "Student's paired t-test",
    "Mixed model ANOVA",
    "Student's one-sample t-test",
    "Welch's two-sample t-test",
    "F-test (ANOVA)"
  )
}

#' Calculate a statistical test for a numerical variable.
#'
#' @param var A variable (a vector).
#' @param group A variable containing the grouping information.
#' @param test_options Named list containing test options.
#' @param test Name of a statistical test.
#' @param var_name Name of variable to be tested (only used in warning messages).
#'
#' @return
#' A list of test test results.
#' @export
#'
#' @examples
#' cont_var <- c(1, 2, 3)
#' test_cont(cont_var)
#' @import dplyr
#' @importFrom magrittr `%<>%`
#' @import tibble
#' @import forcats
#' @import stringr
#' @importFrom utils modifyList
test_cont <-
  function(var,
           group = NULL,
           test_options = list(),
           test = NULL,
           var_name = NULL) {
    # decide how to handle missings
    if (!is.null(group)) {
      paired_test <-
        !is.null(test_options[["indices"]]) &&
          test_options[["paired"]]
      if (paired_test) {
        tibl <-
          tibble(
            var = var,
            group = group,
            id = test_options[["indices"]]
          )
      } else {
        tibl <- tibble(var = var, group = group)
      }

      if (!test_options[["include_group_missings_in_test"]]) {
        tibl %<>% filter(group != "(Missing)")
      }

      tibl %<>% filter(!is.na(var))

      var <- tibl %>% pull(var)
      group <- tibl %>%
        pull(group) %>%
        droplevels()

      if (paired_test) {
        id <- tibl %>% pull(id)
      }

      n_levels_group <- length(levels(group))
    } else {
      tibl <- tibble(var = var)
      tibl %<>% filter(!is.na(var))
      var <- tibl %>% pull(var)
      n_levels_group <- 1
    }

    # if test is not supplied, determine test
    if (is.null(test)) {
      if ((!is.null(group) &&
        !all(table(group) > 1)) || length(var) == 1) {
        warning(
          paste0(
            "Skipping test for variable ",
            var_name,
            " because it has has only 1 nonmissing observation in some group."
          )
        )
        test <- "No appropriate test available."
      } else if (isTRUE(test_options[["nonparametric"]] == TRUE)) {
        # ordinal variable
        if (isTRUE(test_options[["paired"]] == TRUE)) {
          # ordinal variable, paired test
          if (is.null(test_options[["indices"]])) {
            stop(
              "You need to supply patient IDs, e.g. via test_options=list(indices=patIDs)."
            )
          } else {
            if (n_levels_group == 2) {
              test <- "Wilcoxon two-sample signed-rank test"
            } else if (n_levels_group >= 2) {
              test <- "Friedman test"
            }
          }
        } else {
          # ordinal variable, independent test
          if (is.null(group)) {
            test <- "Wilcoxon's one-sample signed-rank test"
          } else if (n_levels_group == 2) {
            test <- "Mann-Whitney's U test"
          } else if (n_levels_group >= 2) {
            test <- "Kruskal-Wallis's one-way ANOVA"
          }
        }
      } else {
        # continuous variable
        if (isTRUE(test_options[["paired"]] == TRUE)) {
          # continuous variable, paired test
          if (is.null(test_options[["indices"]])) {
            stop(
              "You need to supply patient IDs, e.g. via test_options=list(indices=patIDs)."
            )
          } else if (n_levels_group == 2) {
            test <- "Student's paired t-test"
          } else {
            test <- "Mixed model ANOVA"
          }
        } else {
          # continuous variable, independent test
          if (n_levels_group == 1) {
            test <- "Student's one-sample t-test"
          } else if (n_levels_group == 2) {
            test <- "Welch's two-sample t-test"
          } else if (n_levels_group >= 3) {
            test <- "F-test (ANOVA)"
          } else {
            test <- "No appropriate test available."
          }
        }
      }
    }
    if (length(var) == 0) {
      erg <- list(
        p = NA_real_,
        CI = NA_real_,
        CI_name = ""
      )
    } else {
      erg <- switch(test,
        `Wilcoxon two-sample signed-rank test` = {
          good_idx <- names(table(id)[table(id) == 2])
          if (!all(id %in% good_idx)) {
            warning("Removed paired observations with missings.")
          }
          tibl <- tibble(
            var = var,
            group = group,
            id = id
          )
          tibl %<>% filter(id %in% good_idx)
          level1 <- levels(group)[1]
          level2 <- levels(group)[2]

          x <-
            tibl %>%
            filter(group == level1) %>%
            arrange(id) %>%
            pull(var)
          y <-
            tibl %>%
            filter(group == level2) %>%
            arrange(id) %>%
            pull(var)

          arglist <- modifyList(
            list(
              x = x,
              y = y,
              correct = FALSE,
              paired = TRUE
            ),
            as.list(test_options[["additional_test_args"]])
          )

          list(p = ignore_unused_args(stats::wilcox.test, arglist)$p.value)
        },
        `Friedman test` = {
          tmp <-
            tibble(
              var = var,
              group = group,
              idx = id
            )

          arglist <- modifyList(
            list(
              formula = var ~ group | idx,
              data = tmp
            ),
            as.list(test_options[["additional_test_args"]])
          )
          list(p = ignore_unused_args(stats::friedman.test, arglist)$p.value)
        },
        `Wilcoxon's one-sample signed-rank test` = {
          arglist <- modifyList(
            list(
              x = var,
              correct = FALSE
            ),
            as.list(test_options[["additional_test_args"]])
          )
          list(p = ignore_unused_args(stats::wilcox.test, arglist)$p.value)
        },
        `Mann-Whitney's U test` = {
          tmp <- tibble(var = var, group = group)
          arglist <- modifyList(
            list(
              formula = var ~ group,
              conf.int = TRUE,
              correct = FALSE,
              data = tmp
            ),
            as.list(test_options[["additional_test_args"]])
          )
          tl <- ignore_unused_args(stats::wilcox.test, arglist)
          list(
            p = tl$p.value,
            CI = tl$conf.int,
            CI_name = "HL CI"
          )
        },
        `Kruskal-Wallis's one-way ANOVA` = {
          tmp <- tibble(var = var, group = group)
          arglist <- modifyList(
            list(
              formula = var ~ group,
              data = tmp
            ),
            as.list(test_options[["additional_test_args"]])
          )

          list(p = ignore_unused_args(stats::kruskal.test, arglist)$p.value)
        },
        `Student's paired t-test` = {
          good_idx <- names(table(id)[table(id) == 2])
          if (!all(id %in% good_idx)) {
            warning("Removed paired observations with missings.")
          }
          tibl <- tibble(
            var = var,
            group = group,
            id = id
          )
          tibl %<>% filter(id %in% good_idx)
          level1 <- levels(group)[1]
          level2 <- levels(group)[2]

          x <-
            tibl %>%
            filter(group == level1) %>%
            arrange(id) %>%
            pull(var)
          y <-
            tibl %>%
            filter(group == level2) %>%
            arrange(id) %>%
            pull(var)

          arglist <- modifyList(
            list(
              x = x,
              y = y,
              paired = TRUE
            ),
            as.list(test_options[["additional_test_args"]])
          )

          tl <- ignore_unused_args(stats::t.test, arglist)

          list(
            p = tl$p.value,
            CI = tl$conf.int,
            CI_name = "Mean dif. CI"
          )
        },
        `Mixed model ANOVA` = {
          tmp <- tibble(
            var = var,
            group = group,
            idx = id
          )

          arglist <- modifyList(
            list(
              fixed = var ~ group,
              random = ~ 1 | idx,
              data = tmp
            ),
            as.list(test_options[["additional_test_args"]])
          )

          fit <-
            ignore_unused_args(nlme::lme, arglist)
          tl <- stats::anova(fit)
          pv <- tl$`p-value`[2]
          list(p = pv)
        },
        `Student's one-sample t-test` = {
          arglist <- modifyList(
            list(
              x = var
            ),
            as.list(test_options[["additional_test_args"]])
          )

          list(p = ignore_unused_args(stats::t.test, arglist)$p.value)
        },
        `Welch's two-sample t-test` = {
          tmp <- tibble(var = var, group = group)
          arglist <- modifyList(
            list(
              formula = var ~ group,
              var.equal = FALSE,
              data = tmp
            ),
            as.list(test_options[["additional_test_args"]])
          )

          tl <- ignore_unused_args(stats::t.test, arglist)
          list(
            p = tl$p.value,
            CI = tl$conf.int,
            CI_name = "Mean dif. CI"
          )
        },
        `F-test (ANOVA)` = {
          tmp <- tibble(var = var, group = group)
          arglist <- modifyList(
            list(
              formula = var ~ group,
              data = tmp
            ),
            as.list(test_options[["additional_test_args"]])
          )

          tl <- summary(ignore_unused_args(stats::aov, arglist))[[1]]
          pv <- tl$`Pr(>F)`[1]
          list(p = pv)
        },
        list(p = NA_real_)
      )
    }

    erg[["test_name"]] <- test
    erg
  }


#' Calculate a statistical test for a categorical variable.
#'
#' @param var A variable (a vector).
#' @param group A variable containing the grouping information.
#' @param test_options Named list containing test options.
#' @param test Name of a statistical test.
#' @param var_name Name of variable to be tested (only used in warning messages).
#'
#' @return
#' A list of test test results.
#' @export
#'
#' @examples
#' cat_var <- factor(c("a", "b", "c"))
#' test_cat(cat_var)
#' @import dplyr
#' @importFrom magrittr `%<>%`
#' @import tibble
#' @import forcats
#' @import stringr
#' @importFrom DescTools CochranQTest
#' @importFrom utils modifyList
test_cat <-
  function(var,
           group = NULL,
           test_options = list(),
           test = NULL,
           var_name = NULL) {
    # decide how to handle missings
    if (!is.null(group)) {
      paired_test <-
        !is.null(test_options[["indices"]]) &&
          test_options[["paired"]]
      if (paired_test) {
        tibl <-
          tibble(
            var = var,
            group = group,
            id = test_options[["indices"]]
          )
      } else {
        tibl <- tibble(var = var, group = group)
      }

      # tibl <- tibble(var = var, group = group)
      if (!isTRUE(test_options[["include_group_missings_in_test"]])) {
        tibl %<>% filter(group != "(Missing)")
      }
      if (!isTRUE(test_options[["include_categorical_missings_in_test"]])) {
        tibl %<>% filter(var != "(Missing)")
      }

      var <- tibl %>%
        pull(var) %>%
        droplevels()
      group <- tibl %>%
        pull(group) %>%
        droplevels()
      if (paired_test) {
        test_options[["indices"]] <- tibl %>% pull(id)
      }

      n_levels_group <- length(levels(group))
      n_levels_var <- length(levels(var))
    } else {
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
      if (length(levels(var)) == 1) {
        warning(
          paste0(
            "Skipping test for variable ",
            var_name,
            " because it has only 1 level associated with it."
          )
        )
        test <- "No appropriate test available."
      } else if (is.ordered(var)) {
        # ordinal variable
        if (isTRUE(test_options[["paired"]] == TRUE)) {
          # ordinal variable, paired test
          if (is.null(test_options[["indices"]])) {
            stop(
              "You need to supply patient IDs, e.g. via test_options=list(indices=patIDs)."
            )
          } else {
            if (n_levels_group == 2) {
              test <- "Wilcoxon two-sample signed-rank test"
            } else if (n_levels_group >= 2) {
              test <- "Friedman test"
            }
          }
        } else {
          # ordinal variable, independent test
          if (n_levels_group == 1) {
            test <- "Wilcoxon's one-sample signed-rank test"
          } else if (n_levels_group == 2) {
            test <- "Mann-Whitney's U test"
          } else if (n_levels_group >= 2) {
            test <- "Kruskal-Wallis's one-way ANOVA"
          }
        }
      } else {
        # nominal variable
        if (isTRUE(test_options[["exact"]] == TRUE)) {
          # nominal variable, exact test
          if (isTRUE(test_options[["paired"]] == TRUE)) {
            # nominal variable, exact paired test
            if (is.null(test_options[["indices"]])) {
              stop(
                "You need to supply patient IDs, e.g. via test_options=list(indices=patIDs)."
              )
            } else if (n_levels_group == 2 & n_levels_var == 2) {
              test <- "Exact McNemar's test"
            } else {
              test <- "No appropriate test available."
            }
          } else {
            # nominal variable, exact independent test
            if (n_levels_group == 2 & n_levels_var == 2) {
              test <- "Boschloo's test"
            } else {
              test <- "No appropriate test available."
            }
          }
        } else {
          # nominal variable, asymptotic test
          if (isTRUE(test_options[["paired"]] == TRUE)) {
            # nominal variable, asymptotic paired test
            if (is.null(test_options[["indices"]])) {
              stop(
                "You need to supply patient IDs, e.g. via test_options=list(indices=patIDs)."
              )
            } else if (n_levels_group == 2 & n_levels_var == 2) {
              test <- "McNemar's test"
            } else if (n_levels_group >= 3 & n_levels_var == 2) {
              test <- "Cochran's Q test"
            } else {
              test <- "No appropriate test available."
            }
          } else {
            # nominal variable, asymptotic independent test
            if (is.null(group) | n_levels_group == 1) {
              test <- "Chi-squared goodness-of-fit test"
            } else if (n_levels_group >= 2) {
              test <- "Pearson's chi-squared test"
            } else {
              test <- "No appropriate test available."
            }
          }
        }
      }
    }

    if (test %in% c(
      "Friedman test",
      "Wilcoxon two-sample signed-rank test",
      "Wilcoxon's one-sample signed-rank test",
      "Mann-Whitney's U test",
      "Kruskal-Wallis's one-way ANOVA",
      "Student's paired t-test",
      "Mixed model ANOVA",
      "Student's one-sample t-test",
      "Welch's two-sample t-test",
      "F-test (ANOVA)"
    )) {
      var <- as.numeric(as.character(var))
      erg <- test_cont(var, group, test_options, test, var_name)
    } else {
      erg <-
        switch(test,
          `Exact McNemar's test` = {
            # list(p = exact2x2::mcnemar.exact(var, group)$p.value)

            tmp <- tibble(
              var = var,
              group = group,
              idx = test_options[["indices"]]
            )
            tmp1 <-
              tmp %>%
              filter(group == levels(group)[1]) %>%
              arrange("idx")
            tmp2 <-
              tmp %>%
              filter(group == levels(group)[2]) %>%
              arrange("idx")

            if (any(tmp1$idx != tmp2$idx)) {
              stop("Your data is not properly matched. Maybe some pairs contain missings?")
            }
            cont.table <- table(tmp1$var, tmp2$var)

            arglist <- modifyList(
              list(
                x = cont.table,
                alternative = "two.sided",
                tsmethod = "central",
                paired = TRUE
              ),
              as.list(test_options[["additional_test_args"]])
            )

            list(
              p = ignore_unused_args(exact2x2::exact2x2, arglist)$p.value,
              CI = exact2x2::mcnemarExactDP(n = sum(cont.table), x = cont.table[1, 2], m = cont.table[1, 2] + cont.table[2, 1])$conf.int,
              CI_name = "Prop. dif. CI"
            )
          },
          `Boschloo's test` = {
            tabl <- table(var, group)
            x1 <- tabl[1, 1]
            n1 <- sum(tabl[, 1])
            x2 <- tabl[1, 2]
            n2 <- sum(tabl[, 2])
            arglist <- modifyList(
              list(
                x1 = x1,
                n1 = n1,
                x2 = x2,
                n2 = n2
              ),
              as.list(test_options[["additional_test_args"]])
            )

            list(
              p = ignore_unused_args(exact2x2::boschloo, arglist)$p.value,
              CI = stats::prop.test(table(var, group), correct = FALSE)$conf.int,
              CI_name = "Prop. dif. CI"
            )
          },
          `McNemar's test` = {
            tmp <- tibble(
              var = var,
              group = group,
              idx = test_options[["indices"]]
            )
            tmp1 <-
              tmp %>%
              filter(group == levels(group)[1]) %>%
              arrange("idx")
            tmp2 <-
              tmp %>%
              filter(group == levels(group)[2]) %>%
              arrange("idx")

            if (any(tmp1$idx != tmp2$idx)) {
              stop("Your data is not properly matched. Maybe some pairs contain missings?")
            }
            cont.table <- table(tmp1$var, tmp2$var)

            arglist <- modifyList(
              list(
                x = cont.table
              ),
              as.list(test_options[["additional_test_args"]])
            )
            warning("Confidence intervals for differences in proportions ignore the paired structure of the data.
Use Exact McNemar's test if you want confidence intervals which use the test statistic of the
exact McNemar's test.")
            list(
              p = ignore_unused_args(stats::mcnemar.test, arglist)$p.value,
              CI = stats::prop.test(table(var, group), correct = FALSE)$conf.int,
              CI_name = "Prop. dif. CI"
            )
          },
          `Cochran's Q test` = {
            tmp <- tibble(
              var = var,
              group = group,
              idx = test_options[["indices"]]
            )

            arglist <- modifyList(
              list(
                y = var ~ group | idx,
                data = tmp
              ),
              as.list(test_options[["additional_test_args"]])
            )

            list(p = ignore_unused_args(DescTools::CochranQTest, arglist)$p.value)
          },
          `Chi-squared goodness-of-fit test` = {
            arglist <- modifyList(
              list(
                x = table(var),
                correct = FALSE
              ),
              as.list(test_options[["additional_test_args"]])
            )

            list(p = ignore_unused_args(stats::chisq.test, arglist)$p.value)
          },
          `Pearson's chi-squared test` = {
            arglist <- modifyList(
              list(
                x = var,
                y = group,
                correct = FALSE
              ),
              as.list(test_options[["additional_test_args"]])
            )

            if (n_levels_group == 2 & n_levels_var == 2) {
              list(
                p = ignore_unused_args(stats::chisq.test, arglist)$p.value,
                CI = stats::prop.test(table(var, group), correct = FALSE)$conf.int,
                CI_name = "Prop. dif. CI"
              )
            } else {
              list(p = ignore_unused_args(stats::chisq.test, arglist)$p.value)
            }
          },
          list(
            p = NA_real_,
            CI = NA_real_,
            CI_name = ""
          )
        )
    }

    if (is.null(erg[["test_name"]])) {
      erg[["test_name"]] <- test
    }
    erg
  }


#' do.call but without an error for unused arguments
#'
#' @param what either a function or a non-empty character string naming the function to be called.
#' @param args a list of arguments to the function call. The names attribute of args gives the argument names.
#'
#' @return The result of the (evaluated) function call.
#'
#' @examples
#' # works:
#' DescrTab2:::ignore_unused_args(
#'   chisq.test,
#'   list(x = factor(c(1, 0, 1, 1, 1, 0)), y = factor(c(0, 1, 1, 0, 1, 0)), abc = 3)
#' )
#'
#' # would produce error:
#' # do.call(chisq.test, list(x=factor(c(1,0,1,1,1,0)), y=factor(c(0,1,1,0,1,0)), abc=3 ) )
#' @importFrom magrittr `%>%`
ignore_unused_args <- function(what, args) {
  if ("..." %in% names(formals(what))) {
    do.call(what, args %>% as.list())
  } else {
    acceptable_args <- args[names(args) %in% (formals(what) %>% names())]
    do.call(what, acceptable_args %>% as.list())
  }
}



