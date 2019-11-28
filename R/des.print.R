#' @title
#' Create or print an output of a descriptive statistics table
#'
#' @description
#' This is the main function for the user.
#' It can be used to generate a table of descriptive statistics with p-values obtained in tests for
#' difference between the groups. There are five options for the output: pdf, tex, knitr, word file
#' or an Output in R. knitr is not really useful as an output file but for the use within a knitr file.
#'
#' @usage
#' des.print(dat, group, create = "pdf", file, index = T, fsize = 11, paired = F,
#'          nonparametric = F, var.equal = T, correct.cat = F, correct.wilcox = T,
#'          t.log = c(), which.col = c("groups", "total", "p-values"),
#'          group.min.size = F, n.or.miss = c("n", "miss", "miss.cat"), group.miss = F,
#'          percent.vertical = T, var.names, data.names = T, caption, tab.caption,
#'          landscape = F, pos.pagebr = NULL, label = NULL, digits.m = 1,
#'          digits.sd = 2, digits.qu = c(), digits.minmax = 1, digits.p = c(1,2),
#'          silent = T, ...)
#'
#' @param dat
#' Data frame. The data set to be analyzed. Can contain continuous or factor (also ordered) variables.
#' @param group
#' The (optional) grouping variable.\cr
#' Three options to specify it:\cr
#' 1. The index of the group variable in \code{dat}.\cr
#' 2. The variable name. It must be a variable in \code{dat}.\cr
#' 3. As a vector with the same length as the number of rows in \code{dat}.\cr
#' 4. Nothing. Then you need \code{which.col = ("total")}.\cr
#' The specified variable has to be a factor variable with two or more levels.
#' If not specified, a random grouping variable with 2 groups is used.
#' @param create
#' Which output document should be produced (one of "pdf", "tex", "knitr","word" or "R"). Choose "custom" if you add more arguments see \code{...}.
#' @param file
#' File name, which can included the directory (has to have the proper file extension, i.e. .pdf, .tex, or .docx). directory.
#' Only for \code{create == "R"} or \code{"knitr"} isn't a file necessary.
#' @param index
#' Logical. Should the tests used be labeled by footnotes?
#' @param fsize
#' The fontsize in the Output-Document.\cr
#' Default fontsize: For \code{create = "word"} or \code{"R"} fsize = 11 and for \code{create = "pdf"} fsize = 12
#' @param paired
#' Logical. Should paired tests be applied? The groups must have the same length.
#' @param nonparametric
#' Logical or vector of indices. If logical / vector of indices then all / only these continuous variables will be tested using non-parametric methods.
#' @param var.equal
#' Logical. Should variances be assumed to be equal when applying t-tests?
#' @param correct.cat
#' Logical. Should correction be used in chi-sqared tests (see \code{\link{chisq.test}})
#' @param correct.wilcox
#' Logical. Should correction be used in wilcoxon tests (see \code{\link{wilcox.test}})
#' @param t.log
#' Vector of indices: The variables for which the log of the original data
#' should be used when testing for a difference between the groups.
#' @param which.col
#' Which columns should be provided ("groups", "total", "p-values")? Combinations are allowed. "groups" or "total" must be listed. Only "total" and "p-values" is not possible.
#' Type of p-value calculation see \code{\link{p.cat}} (categorical) or \code{\link{p.cont}} (continuous).
#' @param group.min.size
#' For each variable, a p-value is only calculated if each non-empty group contains at least \code{group.min.size} observations for that variable.
#' @param group.non.empty
#' For each variable, a p-value is only calculated if each group contains at least one observation for that variable.
#' @param cat.non.empty
#' For categorical variables a p-value is only calculated if each category is non-empty.
#' @param n.or.miss
#' Should the number of observations, missings for continuous variables, and/or missings for categorical variables be provided ("n", "miss", "miss.cat")? Combinations are allowed.
#' @param group.miss
#' Logical. Should add a column for the Missings in group? For both cases in \code{total} there are only the observations without a missing in group.
#' @param percent.vertical
#' Logical. Should "vertical" percentages for categorical variables be provided?
#' @param var.names
#' Optional. Vector of names to be used in the table for the analyzed variables.
#' @param data.names
#' Logical. If \code{var.names} are specified, should names as saved in \code{dat} be added in brackets?
#' @param caption
#' Optional. Vector of names of the categories of group.\cr
#' If not specified, \code{levels(group)} will be used.
#' @param tab.caption
#' Optional. The caption of the table.\cr
#' If not specified, "Descriptive statistics" will be written.
#' @param landscape
#' Logical. Should the table be in landscape? Only useful for \code{create = "pdf"}.
#' @param pos.pagebr
#' Vector of positions of the pagebreak in tex (or pdf). This is a bit fuzzy. It is the number of lines after a pagebreak should be done.\cr
#' If it is not specified, 45 will be used for "\code{landscape = FALSE}" and 30 will be used for "\code{landscape = TRUE}".
#' @param label
#' Character. If tex (or knitr) in \code{create} is specified, the label of the table.
#' @param digits.m
#' Number of digits for presentation in the table: For mean.
#' @param digits.sd
#' Number of digits for presentation in the table: For standard deviation.
#' @param digits.qu
#' Vector of numbers of digits for presentation in the table: For quantiles (if no value is specified it will be tried to provide a reasonable presentation).
#' @param digits.minmax
#' Number of digits for presentation in the table: For minimum and maximum.
#' @param digits.p
#' Vector with numbers of digits for presentation in the table: For percentages.
#' First vector element is number of digits for the first variable, second element for second variable and so on.
#' @param q.type
#' an integer between 1 and 9 selecting one of the nine quantile algorithms detailed in \code{?quantile} to be used.
#'  @param alignment
#' Specifies where the table shoould be aligned in the ouput document. Default is "c", other options are "l" and "r"
#'
#' @param silent
#' Logical. Should intermediate stages be shown (more for technical reasons)?
#' @param ...
#' further arguments to be passed to or from methods.
#'
#' @details
#' The aim of this function is to help the user to create well-formated descriptive statistics tables.
#' The format can then be fine-tuned in the word, tex or knitr file itself. Only the pdf file is (more or less) unchangeable.
#' Even though it is supposed to be a good starting point for a descriptive analysis, care has to be taken when using the results and a detailed check of the results  might be necessary. For instance, in case of missing values in the group variable and \code{group.miss = FALSE} the respective observations will be omitted also in the total column. A warning will be displayed.
#' If no group variable is specified only the total column (see parameter \code{which.col}) will be returned.
#'
#' @return
#' Only by using \code{create == "R"} an R object will be returned. However, depending on the value of the create parameter either pdf, word, tex or an file optimized for use in connection with knitr will be created containing the descriptive statistics table and saved in a file as specified in the file parameter.
#'
#' @author Lorenz Uhlmann, Csilla van Lunteren
#'
#' @seealso
#' \code{\link{descr}}\cr
#' \code{\link{f.r}}\cr
#' \code{\link{formatr}}\cr
#' \code{\link{inqur}}\cr
#' \code{\link{m.cat}}\cr
#' \code{\link{m.cont}}\cr
#' \code{\link{med.new}}\cr
#' \code{\link{minmax}}\cr
#' \code{\link{p.cat}}\cr
#' \code{\link{p.cont}}\cr
#' \link[flextable]{autofit}\cr
#' \link[flextable]{flextable}\cr
#' \link[flextable]{set_header_df}\cr
#' \link[flextable]{merge_h}\cr
#' \link[flextable]{merge_at}\cr
#' \link[flextable]{align}\cr
#' \link[flextable]{style}\cr
#' \link[flextable]{border}\cr
#' \link[flextable]{bold}\cr
#' \link[flextable]{width}\cr
#' \link[flextable]{height}\cr
#' \link[flextable]{body_add_flextable}\cr
#' \link[officer]{fp_text}\cr
#' \link[officer]{fp_cell}\cr
#' \link[officer]{fp_border}\cr
#' \link[officer]{read_docx}\cr
#' \link[xtable]{xtable}\cr
#' \link[tools]{texi2dvi}\cr
#'
#' @examples
#' \dontrun{
#' infert
#' attach(infert)
#' #is.factor(education)
#' #is.factor(age)
#' #is.factor(parity)
#' #is.factor(induced)
#' #is.factor(case)
#' #infert$case<-as.factor(case)
#' #case<-as.factor(case)
#'
#' #is.factor(spontaneous)
#' #is.factor(stratum)
#' #is.factor(pooled.stratum)
#'
#' #we use case as Grouping variable
#' file <- "C:/Users/lunteren/Desktop/infert/DescriptiveStatisticTable.docx"
#'
#' #Version 1
#' des.print(dat = infert, group = 5, create = "word", file=file, fsize = 10, var.names = c("education", "age", "parity","induced", "spontaneous", "stratum", "pooled.stratum"), caption = c("Group 1", "Group 2"))
#' #Version 2
#' des.print(dat = infert, group = case, create = "word", file=file, fsize = 10, var.names = c("education", "age", "parity","induced", "spontaneous", "stratum", "pooled.stratum"), caption = c("Group 1", "Group 2"))
#' #Version 3
#' group <- case
#' dat <- infert[,-5]
#' des.print(dat = dat, group = group, create = "word", file=file, fsize = 10, var.names = c("education", "age", "parity","induced", "spontaneous", "stratum", "pooled.stratum"), caption = c("Group 1", "Group 2"))
#'
#' ##Dataset with more then two groups
#' ChickWeight
#' attach(ChickWeight)
#'
#' #is.factor(weight)
#' #is.factor(Time)
#' #is.factor(Chick)
#' #is.factor(Diet)
#'
#' #we use Diet as Grouping variable
#'
#' file <- "C:/Users/lunteren/Desktop/ChickWeight/DescriptiveStatisticTable.docx"
#'
#' #Version 1
#' des.print(dat = ChickWeight, group = 4, create = "word", file = file, fsize = 10, var.names = c("weight", "Time", "Chick"), caption = c("Group 1", "Group 2", "Group 3", "Group 4"))
#' #Version 2
#' des.print(dat = ChickWeight, group = Diet, create = "word", file = file, fsize = 10, var.names = c("weight", "Time", "Chick"), caption = c("Group 1", "Group 2", "Group 3", "Group 4"))
#' #Version 3
#' group <- Diet
#' dat <- ChickWeight[,-4]
#' des.print(dat = dat, group = group, create = "word", file = file, fsize = 10, var.names = c("weight", "Time", "Chick"), caption = c("Group 1", "Group 2", "Group 3", "Group 4"))
#' }
#'
#' @importFrom flextable regulartable
#' @importFrom flextable autofit
#' @importFrom flextable flextable
#' @importFrom flextable set_header_df
#' @importFrom flextable merge_h
#' @importFrom flextable merge_at
#' @importFrom flextable align
#' @importFrom flextable style
#' @importFrom flextable border
#' @importFrom flextable bold
#' @importFrom flextable width
#' @importFrom flextable height
#' @importFrom flextable body_add_flextable
#' @importFrom officer fp_text
#' @importFrom officer fp_cell
#' @importFrom officer fp_border
#' @importFrom officer read_docx
#' @importFrom xtable xtable
#' @importFrom tools texi2dvi
#'
#' @export
#'
des.print <- function(dat, group, create = "pdf", file, index = T, fsize = 11,
                      paired = F, nonparametric = F, var.equal = T, correct.cat = F, correct.wilcox = T,
                      t.log = c(), which.col = c("groups", "total", "p-values"), group.min.size = F, group.non.empty=F,
                      cat.non.empty=F, n.or.miss = c("n", "miss", "miss.cat"), group.miss = F,
                      percent.vertical = T, var.names, data.names = T,caption, tab.caption, landscape = F, pos.pagebr = NULL,
                      label = NULL, digits.m = 1, digits.sd = 2, digits.qu = c(),
                      digits.minmax = 1, digits.p = c(1), silent = T, q.type=2, alignment="c", ...) {

  ##Input data correction
  if (!("groups" %in% which.col) & !("total" %in% which.col))
    stop( "At least, either groups or total must be listed in which.col" )

  if (missing(group)) {
    group <- as.factor(rep(1, nrow(dat)))
    which.col <- "total"
    index <- F
    group.miss <- F
    warning( "group is missing! index and group.miss were set to FALSE and which.col were set to \"total\" " )
  }
  if ("p-values" %in% which.col) {
    p.values <- T
  } else {
    p.values <- F
  }

  if (p.values & group.min.size == T)
    group.min.size <- 2

  if (!(p.values) & index) {
    index <- F
    warning( "If you don't want p-values, you don't need an output which test was used. \"index\" was set to FALSE." )
  }

  if ("total" %in% which.col & "p-values" %in% which.col & !("groups" %in% which.col))
    stop( "Only total and p-value is not a useful approach." )

  ##Handling with the group variable
  if (is.numeric(group) & length(group) == 1) {
    gr <- group
    group <- dat[[group]]
    dat <- dat[, -gr, drop = FALSE]
  }

  if (is.character(group)) {
    gr <- which(names(dat) == group)
    group <- dat[[group]]
    dat <- dat[, -gr, drop = FALSE]
  }

  if (is.logical(nonparametric)) {
    if (nonparametric == T) {
      nonparametric <- 1:ncol(dat)
      } else {
        nonparametric <- c()
      }
  }

  ##output settings
  if (missing(dat) | missing(group))
    stop( "Parameters dat and group must be specified" )
  if (create == "word" & !is.numeric(fsize)) {
    warning( "If create=word only whole numbers (numeric variable) are allowed. fsize is set equal to 11." )
    fsize <- 11
  }

  pos.pagebreak <- NULL

  if (length(pos.pagebr) > 1) {
    pos.pagebreak <- pos.pagebr
    pos.pagebr <- NULL
  }

  if (missing(tab.caption)) {
    tab.caption <- "Descriptive statistics"
  }

  ##creation of the output separated for word/R and knitr/pdf/tex
  if (create == "word" | create == "R") {
    ##raw data table
    erg.a <- descr(dat, group, var.names, percent.vertical, data.names, nonparametric, landscape,
                   pos.pagebr, paired, var.equal, correct.cat, correct.wilcox, silent,
                   p.values, group.min.size, group.non.empty, cat.non.empty, n.or.miss, group.miss, t.log, index, create, digits.m,
                   digits.sd, digits.qu, digits.minmax, digits.p, q.type)
    erg <- erg.a$descr[(-(ncol(erg.a$descr) - 2)):(-ncol(erg.a$descr))]
    if (missing(caption))
      caption <- levels(group)

    ##which column shoul be printed
    erg.out <- c()
    if (!("groups" %in% which.col))
      erg.out <- 2:(length(levels(group)) + 1)
    if (!("total" %in% which.col))
      erg.out <- 2 + length(levels(group))
    if (length(erg.out) != 0)
      erg <- erg[, -erg.out]

    names.erg <- c("")
    if ("groups" %in% which.col)
      names.erg <- c(names.erg, caption)
    if ("total" %in% which.col)
      names.erg <- c(names.erg, "Total")
    if (group.miss)
      names.erg <- c(names.erg, "Missing in group")
    if ("p-values" %in% which.col)
      names.erg <- c(names.erg, "p-values")
    names(erg)<-names.erg

    ##numbers per group
    n.vec <- c()
    for (j in 1:length(levels(group))) {
      n.j <- paste("(", "n = ", length(group[which(group == levels(group)[j])]), ")", sep = "")
      n.vec <- c(n.vec, n.j)
    }
    n.total <- paste("(", "n = ", length(group), ")", sep = "")
    if (group.miss)
      n.miss<- paste("(", "n = ",length(which(is.na(group))), ")", sep = "")
    for (k in 1:ncol(erg)) {
      erg[, k] <- as.character(erg[, k])
    }

    ##fontsize calculation
    difference <- 11 - fsize
    fsizeFoo <- 10 - difference
    width <- 11 / fsize

    ##footline & output creation
    if (index & "p-values" %in% which.col) {
      if (!is.null(erg.a$testings)) {
        foot.ab <- paste(letters[1], erg.a$testings[1], sep = ":")
        if (length(erg.a$testings) >= 2) {
          for (k in 2:length(erg.a$testings)) {
            foot.ab <- paste(foot.ab, paste(letters[k], erg.a$testings[k], sep = ":"), sep = "; ")
          }
        }
      }else{
        foot.ab <- ""
      }

      a <- grep("a", erg$`p-values`)
      b <- grep("b", erg$`p-values`)
      erg$`p-values` <- gsub("a", "", erg$`p-values`)
      erg$`p-values` <- gsub("b", "", erg$`p-values`)

      footsuperscript <- rep("", nrow(erg))
      footsuperscript[a] <- "a"
      footsuperscript[b] <- "b"
      erg <- cbind(erg, footsuperscript)
      foot <- rep("", ncol(erg) - 1)
      foot <- c(foot.ab, foot)
      erg <- rbind(erg, foot)
    }

    header <- c("")
    if ("groups" %in% which.col)
      header <- c(header, n.vec)
    if ("total" %in% which.col)
      header <- c(header, n.total)
    if (group.miss)
      header <- c(header, n.miss)
    if ("p-values" %in% which.col)
      header <- c(header, "")

    jet <- erg
    names(jet)[1] <- "var"

    if("p-values" %in% which.col)
      names(jet)[which(names(jet) == "p-values")] <- "p.values"
    if (index)
      names(jet)[ncol(jet)] <- "index"

    jet <- flextable::regulartable(data = jet)
    jet <- flextable::autofit(jet)

    if (index == F) {
      head <- data.frame(
        col_keys = jet$col_keys,
        tab = tab.caption,
        what = names.erg,
        measure  =header,
        stringsAsFactors = F
      )
    } else {
      head <- data.frame(
        col_keys = jet$col_keys,
        tab = tab.caption,
        what = c(names.erg, ""),
        measure = c(header, ""),
        stringsAsFactors = F
      )
    }

    flextable::autofit(flextable::flextable(head))
    jet <- flextable::set_header_df(jet, mapping = head, key = "col_keys")
    jet <- flextable::merge_h(jet, i = 1, part = "header")
    if (index)
      jet <- flextable::merge_at(jet, i = nrow(erg), j = 1:ncol(erg))
    jet <- flextable::align(jet, j = 1, align = "left", part = "all")
    if ("p-values" %in% which.col & index){
      jet <- flextable::align(jet, j = 2:(ncol(erg) - 2), align = "center", part = "all")
      jet <- flextable::align(jet, j = ncol(erg) - 1, align = "right", part = "all")
      jet <- flextable::align(jet, j = ncol(erg), align = "left", part = "all")
    } else if ("p-values" %in% which.col & !index){
      jet <- flextable::align(jet, j = 2:(ncol(erg) - 1), align = "center", part = "all")
      jet <- flextable::align(jet, j = ncol(erg), align = "right", part = "all")
    } else {
      jet <- flextable::align(jet, j = 2:ncol(erg), align = "center", part = "all")
    }

    jet <- flextable::style(jet, pr_t = officer::fp_text(font.family="Cambria (Textkörper)", font.size = fsize, bold = TRUE), part = "header")
    jet <- flextable::style(jet, pr_t = officer::fp_text(font.family="Cambria (Textkörper)", font.size = fsize), part = "body")
    if (index)
      jet <- flextable::style(jet, pr_t = officer::fp_text(font.family="Cambria (Textkörper)", font.size = fsizeFoo), i = nrow(erg))
    if ("p-values" %in% which.col & index)
      jet <- flextable::style(jet, j = ncol(erg), pr_t = officer::fp_text(vertical.align = "superscript"))

    def_cell <- officer::fp_cell(border = officer::fp_border(color = "transparent"))
    jet <- flextable::style( jet, pr_c = def_cell, part = "body")
    jet <- flextable::border(x = jet, i = 1, border.top = officer::fp_border(width = 2), part = "header")
    if (is.null(jet[["header"]])) {
      hh <- 0
    } else if( is.null(jet[["header"]]$dataset) ){
      hh <- 0
    } else {
      hh <- nrow(jet[["header"]]$dataset)
    }
    jet <- flextable::border(x = jet, i = hh, border.bottom = officer::fp_border(width = 2), part = "header")
    jet <- flextable::bold(x = jet, bold = TRUE, part = "header")
    jet <- flextable::border(x = jet, i = nrow(erg), border.top = officer::fp_border(width = 2))
    jet <- flextable::width(jet, width = 1.3 * (fsize / 11), j = 1)

    if ("p-values" %in% which.col & index) {
      if (ncol(erg)>2) {
        jet <- flextable::width(jet, width = 1.3 * (fsize / 11), j = 2:(ncol(erg) - 2))
        jet <- flextable::width(jet, width = .7 * (fsize / 11), j = ncol(erg) - 1)
        jet <- flextable::width(jet, width = .1 * (fsize / 11), j = ncol(erg))
      } else {
        jet <- flextable::width(jet, width = 1.3 * (fsize / 11), j = 2)
      }
    } else if ("p-values" %in% which.col & !index) {
      if (ncol(erg)>2) {
        jet <- flextable::width(jet, width = 1.3 * (fsize / 11), j = 2:(ncol(erg) - 1))
        jet <- flextable::width(jet, width = .7 * (fsize / 11), j = ncol(erg))
      } else {
        jet <- flextable::width(jet, width = 1.3 * (fsize / 11), j = 2)
      }
    } else {
      jet <- flextable::width(jet, width = 1.3 * (fsize / 11), j = 2)
    }
    jet <- flextable::height(jet, height = .25 * (fsize / 11), part = "body")
    jet <- flextable::height(jet, height = .25 * (fsize / 11), part = "header")

    if (create == "word") {
      my_doc <- officer::read_docx()

      if (alignment=="l"){
        my_doc <- flextable::body_add_flextable(my_doc, jet, align = "left")
      }
      else if (alignment=="r"){
        my_doc <- flextable::body_add_flextable(my_doc, jet, align = "right")
      }
      else{
        my_doc <- flextable::body_add_flextable(my_doc, jet)
      }

      print(my_doc, target = file)
    } else if (create == "R") {
      return(jet)
    }
  } else if (create %in% c("pdf", "knitr", "tex", "custom")) {
    ##pdf, knitr, tex, custom

    ##tex language
    r.s <- c("%", "{", "}", "&", "#")
    r.s.a <- c(">", "<", "|")
    if (!(missing(var.names))) {
      for (i in 1:length(r.s))
        var.names <- gsub(r.s[i], paste("\\\\", r.s[i], sep = ""), var.names, fixed = T)
      for (i in 1:length(r.s.a))
        var.names <- gsub(r.s.a[i], paste("$", r.s.a[i], "$", sep = ""), var.names, fixed = T)
      var.names <- gsub("^", "\\verb|^|", var.names, fixed = T)
      var.names <- gsub("~", "\\~{}", var.names, fixed = T)
      var.names <- gsub("_", "\\_", var.names, fixed = T)
    }

    for (i in 1:length(r.s))
      names(dat) <- gsub(r.s[i], paste("\\\\", r.s[i], sep = ""), names(dat), fixed = T)
    for (i in 1:length(r.s.a))
      names(dat) <- gsub(r.s.a[i], paste("$", r.s.a[i], "$", sep = ""), names(dat), fixed = T)
    names(dat) <- gsub("^", "\\verb|^|", names(dat), fixed = T)
    names(dat) <- gsub("~", "\\~{}", names(dat), fixed = T)
    names(dat) <- gsub("_", "\\_", names(dat), fixed = T)

    ##raw data table
    erg.a <- descr(dat, group, var.names, percent.vertical, data.names, nonparametric, landscape,
                   pos.pagebr, paired, var.equal, correct.cat, correct.wilcox, silent,
                   p.values, group.min.size, group.non.empty, cat.non.empty, n.or.miss, group.miss, t.log, index, create, digits.m,
                   digits.sd, digits.qu, digits.minmax, digits.p, q.type)
    erg <- erg.a$descr[(-(ncol(erg.a$descr) - 2)):(-ncol(erg.a$descr))]

    ##which column shoul be printed
    erg.anz <- 0
    erg.out <- c()
    if ("groups" %in% which.col) {
      erg.anz <- erg.anz + length(levels(group))
    } else {
      erg.out <- 2:(length(levels(group)) + 1)
    }
    if ("total" %in% which.col) {
      erg.anz <- erg.anz + 1
    } else {
      erg.out <- 2 + length(levels(group))
    }
    if (group.miss)
      erg.anz <- erg.anz + 1
    erg.align <- c("l", "l",  rep("c", erg.anz))
    if (length(erg.out) != 0)
      erg <- erg[, -erg.out]

    if ("p-values" %in% which.col) {
      ab.t <- xtable::xtable(erg, align = c(erg.align, "r"), caption = tab.caption, label = label)
    } else {
      ab.t <- xtable::xtable(erg, align = c(erg.align), caption = tab.caption, label = label)
    }

    ##tex language
    if (missing(caption))
      caption <- levels(group)

    for (i in 1:length(r.s))
      caption <- gsub(r.s[i], paste("\\\\", r.s[i], sep = ""), caption, fixed = T)
    for (i in 1:length(r.s.a))
      caption <- gsub(r.s.a[i], paste("$", r.s.a[i], "$", sep = ""), caption, fixed = T)
    caption <- gsub("^", "\\verb|^|", caption, fixed = T)
    caption <- gsub("~", "\\~{}", caption, fixed = T)

    pos <- list(0)

    ##numbers per group
    n.vec <- c()
    for (j in 1:length(levels(group))) {
      n.j <- paste("(", "n = ", length(group[which(group == levels(group)[j])]), ")", sep = "")
      n.vec <- c(n.vec, n.j)
    }
    n.total <- paste("(", "n = ", length(group), ")", sep = "")
    if(group.miss)
      n.miss <- paste("(", "n = ",length(which(is.na(group))), ")", sep = "")

    ##footline & output creation
    if (index) {
      foot.ab <- paste("$^", letters[1], "$", erg.a$testings[1], "\\quad", sep = "")
      if (length(erg.a$testings) >= 2) {
        for (k in 2:length(erg.a$testings)) {
          foot.ab <- paste(foot.ab, "$^", letters[k], "$", erg.a$testings[k], "\\quad", sep = "")
        }
      }
    }

    ##line labeling
    command <- "\\hline"
    if ("groups" %in% which.col)
      command <-  paste(command, paste(caption, collapse = " & "), sep = " & ")
    if ("total" %in% which.col)
      command <- paste(command, "Total ", sep = " & ")
    if (group.miss)
      command <- paste(command, " Missing in group ", sep = " & ")
    if ("p-values" %in% which.col)
      command <- paste(command, "\\hspace{1ex} p-value ", sep = " & ")
    command <- paste(command, " \\\\")
    if ("groups" %in% which.col)
      command <- paste(command, paste(n.vec, collapse = " & "), sep = " & ")
    if ("total" %in% which.col)
      command <- paste(command, n.total, sep = " & ")
    if (group.miss)
      command <- paste(command, n.miss, sep = " & ")

    if (index) {
      command <- paste(command, paste(
        "\\\\
        \\hline\\\\  \\endhead
        \\hline\\noalign{
        \\smallskip
        }",foot.ab,"\\endfoot ", sep = ""))

    }else{
      command <- paste(command, paste("\\\\
                                      \\hline\\\\
                                      \\endhead
                                      \\hline\\\\",
                                      "\\endfoot ", sep = ""))
    }

    if (is.null(pos.pagebreak) & !is.null(erg.a$pos)) {
      for (i in 1:length(erg.a$pos))
        pos[[i + 1]] <- erg.a$pos[i]
      command <- c(command, rep("\\pagebreak ", length(erg.a$pos)))
    } else {
      if (!is.null(pos.pagebreak)) {
        for (i in 1:length(pos.pagebreak))
          pos[[i + 1]] <- pos.pagebreak[i]
        command <- c(command, rep("\\pagebreak ", length(pos.pagebreak)))
      }
    }

    pc <- list("pos" = pos, "command" = command)

    if (create == "pdf") {
      ##fontsize calculation
      if (is.numeric(fsize)) {
        if (fsize <= 6) {
          fsizep <- "tiny"
          fsizec <- "scriptsize"
        }
        if (fsize > 6 & fsize <= 8)
          fsizep <- fsizec <- "scriptsize"
        if (fsize == 9)
          fsizep <- fsizec <- "footnotesize"
        if (fsize == 10)
          fsizep <- fsizec <- "small"
        if (fsize == 11)
          fsizep <- fsizec <- "normalsize"
        if (fsize == 12)
          fsizep <- fsizec <- "large"
        if (fsize > 12 & fsize <= 15)
          fsizep <- fsizec <- "Large"
        if (fsize > 15 & fsize <= 18) {
          fsizep <- "LARGE"
          fsizec <- "Large"
        }
        if (fsize > 18 & fsize <= 22) {
          fsizep <- "huge"
          fsizec <- "Large"
        }
        if (fsize > 22) {
          fsizep <- "Huge"
          fsizec <- "Large"
        }
      } else {
        fsizep <- fsizec <- fsize
        if (fsize == "tiny")
          fsizec <- "scriptsize"
        if (fsize %in% c("LARGE", "huge", "Huge"))
          fsizec <- "Large"
      }

      ##output creation
      if (landscape) {
        cat(paste("\\documentclass[landscape]{report} \n
                  \\usepackage[T1]{fontenc}\n
                  \\usepackage{longtable}
                  \\usepackage{a4wide}\n
                  \\usepackage[landscape]{geometry}
                  \\usepackage[justification=RaggedRight, singlelinecheck=off,
                  margin = 1cm, labelfont=bf, skip=4pt, font=", fsizec, "]{caption}\n
                  \\begin{document}\n\\", fsizep,
                  "\\input{t.tex}\n
                  \\end{document}", sep = ""), file = "a.tex")
      } else {
        cat(paste("\\documentclass{report} \n
                  \\usepackage[T1]{fontenc}\n
                  \\usepackage{longtable}
                  \\usepackage{a4wide}\n
                  \\usepackage[justification=RaggedRight, singlelinecheck=off,
                  margin = 1cm, labelfont=bf, skip=4pt, font=", fsizec, "]{caption}\n
                  \\begin{document}\n\\", fsizep,
                  "\\input{t.tex}\n
                  \\end{document}", sep = ""), file = "a.tex")
      }




       tmp.ltx <- capture.output(print(ab.t,  type = "latex", include.colnames = F, include.rownames = F,
                                            tabular.environment = "longtable",
                                       sanitize.text.function = function(x){x}, floating = F,
                                            hline.after = NULL, add.to.row = pc, caption.placement = "top", ...))
      if (alignment=="c"){

      }
      else if (alignment=="l"){
        tmp.ltx <- stringr::str_replace(tmp.ltx, stringr::fixed("\\begin{longtable}"), "\\begin{longtable}[l]")
      }
      else if (alignment=="r"){
        tmp.ltx <- stringr::str_replace(tmp.ltx, stringr::fixed("\\begin{longtable}"), "\\begin{longtable}[r]")
      }
      cat(tmp.ltx, sep="\n", file="t.tex")



      tools::texi2dvi("a.tex", pdf = T, clean = T, texi2dvi = "")

      file.rename("a.pdf", file)

      file.remove("a.tex")
      file.remove("t.tex")
    }
    if (create == "tex") {
      tmp.ltx <- capture.output(print(ab.t, file = file, type = "latex", include.colnames = F, include.rownames = F,
                                      tabular.environment = "longtable", sanitize.text.function = function(x){x}, floating = F,
                                      hline.after = NULL, add.to.row = pc, caption.placement = "top"))
      if (alignment=="c"){

      }
      else if (alignment=="l"){
        tmp.ltx <- stringr::str_replace(tmp.ltx, stringr::fixed("\\begin{longtable}"), "\\begin{longtable}[l]")
      }
      else if (alignment=="r"){
        tmp.ltx <- stringr::str_replace(tmp.ltx, stringr::fixed("\\begin{longtable}"), "\\begin{longtable}[r]")
      }
      cat(tmp.ltx, sep="\n")
    }
    if (create == "knitr") {
      tmp.ltx <- capture.output(print(ab.t, type = "latex", include.colnames = F, include.rownames = F, latex.environments = "left",
            tabular.environment = "longtable", sanitize.text.function = function(x){x}, floating = F,
            hline.after = NULL, add.to.row = pc, caption.placement = "top", comment = FALSE))
      if (alignment=="c"){

      }
      else if (alignment=="l"){
        tmp.ltx <- stringr::str_replace(tmp.ltx, stringr::fixed("\\begin{longtable}"), "\\begin{longtable}[l]")
      }
      else if (alignment=="r"){
        tmp.ltx <- stringr::str_replace(tmp.ltx, stringr::fixed("\\begin{longtable}"), "\\begin{longtable}[r]")
      }
      cat(tmp.ltx, sep="\n")
    }
    if (create == "custom") {
      print(ab.t, type = "latex", include.colnames = F, include.rownames = F,
            sanitize.text.function = function(x){x}, hline.after = NULL, add.to.row = pc, ...)
    }
  }
  else if (create == "archive"){
    erg.a <- descr(dat, group, var.names, percent.vertical, data.names, nonparametric, landscape,
                   pos.pagebr, paired, var.equal, correct.cat, correct.wilcox, silent,
                   p.values, group.min.size, group.non.empty, cat.non.empty, n.or.miss, group.miss, t.log, index, create, digits.m,
                   digits.sd, digits.qu, digits.minmax, digits.p, q.type)


    n.vec <- c()
    for (j in 1:length(levels(group))) {
      n.j <- paste(" (", "n = ", length(group[which(group == levels(group)[j])]), ")", sep = "")
      n.vec <- c(n.vec, n.j)
    }

    if (missing(caption))
      caption <- levels(group)

    n.total <- paste(" (", "n = ", length(group), ")", sep = "")
    if (group.miss)
      n.miss<- paste(" (", "n = ",length(which(is.na(group))), ")", sep = "")

    names.erg <- c("")
    if ("groups" %in% which.col)
      names.erg <- c(names.erg, stringr::str_c(caption, n.vec))
    if ("total" %in% which.col)
      names.erg <- c(names.erg, stringr::str_c("Total", n.total))
    if (group.miss)
      names.erg <- c(names.erg, stringr::str_c("Missing in group", n.miss))
    if ("p-values" %in% which.col)
      names.erg <- c(names.erg, "p_formatted", "p_val", "test_val", "test_name")



    names(erg.a$descr) <- names.erg
    if (!missing(file)){
      write.csv(erg.a$descr, file = file)
    }

    return(erg.a$descr)
  }
}
