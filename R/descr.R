#' @title Create a descriptive statistics table
#'
#' @description
#' Generate a table of descriptive statistics with p-values obtained in tests for difference between the groups.
#'
#' @usage
#' descr(dat, group, var.names, percent.vertical = T, data.names = T, nonparametric = c(), landscape = F,
#'       pos.pagebr = NULL, paired = F, var.equal = T, correct.cat = F, correct.wilcox = T, silent = T,
#'       p.values = T, groupsize = F, n.or.miss = "n", group.miss = F, t.log = c(), index = T,
#'       create = "tex", digits.m = 1, digits.sd = 2, digits.qu = c(), digits.minmax = 1, digits.p = 1)
#'
#' @param dat
#' Data frame. The data set to be analyzed. Can contain continuous or factor (also ordered) variables.
#' @param group
#' Vector of the grouping variable.
#' @param var.names
#' Optional. Vector of names to be used in the table for the analyzed variables.
#' @param percent.vertical
#' Logical. Should "vertical" percentages for categorical variables be provided?
#' @param data.names
#' Logical. If \code{var.names} are specified, should names as saved in \code{dat} be added in brackets?
#' @param nonparametric
#' Logical or vector of indices. If logical / vector of indices then all / only these continuous variables will be tested using non-parametric methods.
#' @param landscape
#' Logical. Should the table be in landscape? Only useful if you want create a "pdf"- or "knitr"-document in the following. (see \code{pos.pagebr})
#' @param pos.pagebr
#' Vector of positions of the pagebreak in tex (or pdf). This is a bit fuzzy. It is the number of lines after a pagebreak should be done.\cr
#' If it is not specified, 45 will be used for "\code{landscape=FALSE}" and 30 will be used for "\code{landscape=TURE}".\cr
#' Only useful if you want know the number for a pagebreak when you create a "pdf"- or "knitr"-document in the following.
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
#' @param groupsize
#' Logical. Should be checked for each variable whether the groups contain at least two cases.
#' Number.Instead of two any other number.
#' @param n.or.miss
#' Should the number of observations, missings for continuous variables, and/or missings for categorical variables be provided ("n", "miss", "miss.cat")? Combinations are allowed.
#' @param group.miss
#' Logical. Schould add a column for the Missings in group?
#' @param t.log
#' Vector of indices: The variables for which the log of the original data should be used when testing for a difference between the groups.
#' @param index
#' Logical. Should the tests used be labeled by footnotes? Only usefull if "p-values" in \code{which.col}.
#' @param create
#' Which output document should be produced in the following step (one of "pdf", "tex", "knitr", "word" or "R").
#' @param digits.m
#' Number of digits for presentation in the table: For mean.
#' @param digits.sd
#' Number of digits for presentation in the table: For standard deviation.
#' @param digits.qu
#' Vector of numbers of digits for presentation in the table: For quantiles (if no value is specified it will be tried to provide a reasonable presentation).
#' @param digits.minmax
#' Number of digits for presentation in the table: For minimum and maximum.
#' @param digits.p
#' Number of digits for presentation in the table: For percentages.
#'
#' @return
#' Depending on the value of the create parameter either pdf, word, tex, R or an file optimized for use in connection with knitr will be created containing the descriptive statistics table with the speak for the document to create in the following.
#' For example you choose \code{create="pdf"} then the table is written in \code{TeX}-Code.
#' Attention: the table has no caption and numbers of observations per group.
#'
#' @author Lorenz Uhlmann, Csilla van Lunteren
#'
#' @seealso
#' \code{\link{med.new}}
#' \code{\link{inqur}}
#' \code{\link{minmax}}
#' \code{\link{f.r}}
#' \code{\link{formatr}}
#' \code{\link{m.cat}}
#' \code{\link{m.cont}}
#' \code{\link{p.cat}}
#' \code{\link{p.cont}}
#'
#' @examples
#' \dontrun{
#' ##Dataset with two groups
#' infert
#' attach(infert)
#'
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
#'
#' #Version 1
#' descr(dat = infert, group = case, var.names = c("education", "age", "parity", "induced", "spontaneous", "stratum", "pooled.stratum"),create = "word")
#' #Version 2
#' group <- case
#' dat <- infert[,-5]
#' descr(dat = dat, group = group, var.names = c("education", "age", "parity", "induced", "spontaneous", "stratum", "pooled.stratum"),create = "word")
#'
#'
#' ##Dataset with more then two groups
#'
#' ChickWeight
#' attach(ChickWeight)
#' #is.factor(weight)
#' #is.factor(Time)
#' #is.factor(Chick)
#' #is.factor(Diet)
#'
#' #we use Diet as Grouping variable
#'
#' #Version 1
#' descr(dat = ChickWeight, group = Diet, var.names = c("weight", "Time", "Chick"), create = "word")
#'
#' #Version 4
#' group <- Diet
#' dat <- ChickWeight[,-4]
#' descr(dat = dat, group = group, var.names = c("weight", "Time", "Chick"), create = "word")
#' }
#'
#' @export
#'
descr <- function(dat, group, var.names, percent.vertical = T, data.names = T, nonparametric = c(), landscape = F,
                  pos.pagebr = NULL, paired = F, var.equal = T, correct.cat = F, correct.wilcox = T, silent = T,
                  p.values = T, groupsize = F, n.or.miss = "n", group.miss = F, t.log = c(), index = T, create = "tex", digits.m = 1,
                  digits.sd = 2, digits.qu = c(), digits.minmax = 1, digits.p = 1) {

  if (is.null(nonparametric))
    nonparametric <- rep(F, ncol(dat))
  if (is.null(t.log))
    t.log <- rep(F, ncol(dat))
  l.i <- 0

  testings <- c()
  pos <- c()
  pos.i <- 0
  pos.mult <- 1
  index_var <- c()

  datmiss <- matrix(NA, nrow = nrow(dat), ncol = ncol(dat))
  group.na.index <- which(is.na(group))

  if (group.miss) {
    groupmiss <- as.numeric(group)
    groupmiss[group.na.index] <- "NA"
    datmiss <- dat
  }
  if (length(group.na.index) != 0) {
    warning(paste("Missing values in the group variable ( Observations: ", list(group.na.index), " )! Observations will be removed!"))
    dat <- dat[-group.na.index, ]
    group <- group[-group.na.index]
  }

  lgr <- length(levels(group))
  for (i in 1:ncol(dat)) {
    gr.miss <- c()
    if (is.factor(dat[ ,i])) {
      a <- table(dat[ ,i], group)
      if (group.miss & length(group.na.index) != 0)
        gr.miss <- c(gr.miss, table(datmiss[ ,i], groupmiss)[ ,which(colnames(table(datmiss[ ,i], groupmiss)) == "NA")])
      if (group.miss & length(group.na.index) == 0)
        gr.miss <- rep(0,length(levels(dat[,i])))
      d <- table(dat[ ,i])
      if (percent.vertical == T) {
        b <- prop.table(as.matrix(table(dat[ ,i], group)), 2)
        e <- prop.table(as.matrix(table(dat[ ,i])), 2)
        ab <- data.frame(cbind(a[ ,1], b[ ,1]*100))
        if (ncol(a) >= 2) {
          for (k in 2:ncol(a))
            ab <- data.frame(cbind(ab, a[ ,k], b[ ,k]*100))
        }
        if (group.miss) {
          ab <- data.frame(cbind(ab, as.vector(d) + gr.miss, as.vector(prop.table((as.matrix(table(dat[ ,i])) + gr.miss), 2))*100))
        } else {
          ab <- data.frame(cbind(ab, as.vector(d), as.vector(e) * 100))
        }
        if (create == "word" | create == "R") {
          for (j in seq(1, 2 * (length(levels(group))) + 1, by = 2)) {
            for (k in 1:nrow(ab)) {
              if (ab[k, j] != 0 ) {
                ab[k, j] <- paste(ab[k, j], " (", formatr(ab[k, (j + 1)], digits.p), "%)", sep = "")
              } else {
                ab[k, j] <- paste(ab[k, j], " ( - ) ")
              }
            }
          }
        } else {
          for (j in seq(1, 2 * (length(levels(group))) + 1, by = 2)) {
            for (k in 1:nrow(ab))
              ab[k, j] <- paste(ab[k, j], " (", formatr(ab[k, (j + 1)], digits.p), "\\%)", sep = "")
          }
        }
      } else {
        b <- prop.table(as.matrix(table(dat[, i], group)), 1)
        ab <- data.frame(cbind(a[, 1], b[, 1] * 100))
        if (ncol(a) >= 2) {
          for (k in 2:ncol(a)) {
            ab <- data.frame(cbind(ab, a[, k], b[, k] * 100))
          }
        }
        if (group.miss) {
          ab <- data.frame(cbind(ab, as.vector(d) + gr.miss))
        } else {
          ab <- data.frame(cbind(ab, as.vector(d)))
        }
        if (create == "word" | create == "R") {
          for (j in seq(1, 2 * (length(levels(group))), by = 2)) {
            for (k in 1:nrow(ab)) {
              ab[k, j] <- paste(ab[k, j], " (", formatr(ab[k, (j + 1)], digits.p), "%)", sep = "")
            }
          }
        } else {
          for (j in seq(1, 2 * (length(levels(group))), by = 2)) {
            for (k in 1:nrow(ab)) {
              ab[k, j] <- paste(ab[k, j], " (", formatr(ab[k, (j + 1)], digits.p), "\\%)", sep = "")
            }
          }
        }
      }

      index.delete <- (1:(length(levels(group)) + 1)) * 2
      ab <- ab[, -index.delete]

      if ("miss.cat" %in% n.or.miss) {
        miss.end <- c()
        for (j in 1:lgr) {
          miss.j <- length(which(is.na(dat[which(group == levels(group)[j]), i])))
          miss.end <- c(miss.end, miss.j)
        }
        miss.all <- length(which(is.na(dat[ ,i])))
        ab <- rbind(ab, c(miss.end, miss.all))
        if (group.miss) {
          gr.miss <- c(gr.miss, length(which(is.na(datmiss[which(groupmiss == "NA"), i]))))
        }
      }

      ab <- rbind(rep("", lgr + 1), ab, rep("", lgr + 1))
      if (group.miss)
        gr.miss <- c("", gr.miss, "")

      if (!(missing(var.names))) {
        if (data.names == T) {
          var.n1 <- var.names[i]
          var.n2 <- paste(paste("(", names(dat)[i], sep = ""), ")", sep = "")
          var.n <- paste(var.n1, var.n2, sep = " ")
        } else {
          var.n <- var.names[i]
        }
      } else {
        var.n <- names(dat)[i]
      }
      ab <- cbind(NA, ab)


      ab <- as.data.frame(ab)
      levels(dat[,i]) <- paste(" ", levels(dat[,i]))
      if (create == "R")
        levels(dat[,i]) <- paste("- ", levels(dat[,i]))
      if ("miss.cat" %in% n.or.miss) {
        if (create == "R"){
          ab[ ,1] <- c(var.n, levels(dat[ ,i]), "- Missing", "")
        } else {
          ab[ ,1] <- c(var.n, levels(dat[ ,i]), "  Missing", "")
        }
      } else {
        ab[ ,1] <- c(var.n, levels(dat[ ,i]), "")
      }
      if (create != "word" & create != "R") {
        for (j in 1:(length(levels(dat[, i])) + ("miss.cat" %in% n.or.miss))) {
          ab[j + 1, 1] <- paste("\\hspace{2ex}", ab[j + 1, 1])
        }
      } else {
        for (j in 1:(length(levels(dat[ ,i])) + ("miss.cat" %in% n.or.miss))) {
          ab[j + 1, 1] <- paste(" ", ab[j + 1, 1])
        }
      }
      if (group.miss) {
        ab <- cbind(ab, gr.miss)
      }

      pvalues_var <- matrix(F, ncol = ncol(dat))
      pvalues_var[i] <- p.values
      if (pvalues_var[i]) {
        a <- dat[ ,i]
        a.list <- list()
        a.list[[(length(levels(group)) + 1)]] <- na.omit(a)
        n.vector <- c()
        for (k in 1:length(levels(group))) {
          a.list[[k]] <- na.omit(a[which(group == levels(group)[k])])
          n.vector <- c(n.vector, length(a.list[[k]]))
        }
        for (l in 1:length(levels(group))) {
          if (n.vector[l] < groupsize) {
            pvalues_var[i] <- F
          }
        }
        if (length(table(dat[,i])) == 1) {
          pvalues_var[i] = F
        } else {
          for (l in 1:length(table(dat[,i]))) {
            if(table(dat[,i])[l] == 0) {
              pvalues_var[i] = F
            }
          }
        }

      if (pvalues_var[i]) {
        if (index) {
          m <- m.cat(group, paired = paired, is.ordered = is.ordered(dat[, i]))
          if (!(m %in% testings)) {
            testings <- c(testings, m)
            l.i <- l.i + 1
            index.i <- letters[l.i]
          }
        } else {
          index.i <- c()
        }
        pv <- p.cat(dat[ ,i], group, paired = paired, is.ordered = is.ordered(dat[ ,i]),
                    correct.cat = correct.cat, correct.wilcox = correct.wilcox, index = index.i,
                    create = create)
        index_var[i] <- T
      } else {
        index_var[i] <- F
        pv <- "--"
      }
      ab <- cbind(ab, c("", pv, rep("", length(levels(dat[, i])) + ("miss.cat" %in% n.or.miss))))
      }

      pos.i.alt <- pos.i

      pos.i <- pos.i + length(levels(dat[ ,i])) + 2 + length(n.or.miss)
    } else {
      a <- dat[, i]
      a.list <- list()
      a.list[[(length(levels(group)) + 1)]] <- na.omit(a)

      n.vector <- c()
      for (k in 1:length(levels(group))) {
        a.list[[k]] <- na.omit(a[which(group == levels(group)[k])])
        n.vector <- c(n.vector, length(a.list[[k]]))
      }
      n.vector <- c(n.vector, length(na.omit(a)))

      n.miss <- c()
      for (k in 1:length(levels(group))) {
        miss.k <- which(is.na(a[which(group == levels(group)[k])]))
        n.miss <- c(n.miss, length(miss.k))
      }
      n.miss <- c(n.miss, length(which(is.na(a))))

      ab <- matrix(NA, nrow = 6, ncol = (length(levels(group)) + 1))

      if(group.miss) {
        a.miss <- datmiss[ ,i]
        a.miss <- na.omit(a.miss)
        a.list.miss <- na.omit(a.miss[which(groupmiss=="NA")])

        n.vector.miss <- length(a.list.miss)

        miss.k.miss <- which(is.na(a.miss[which(groupmiss == "NA")]))
        n.miss.miss <- c(length(miss.k.miss))

        n.vector <- c(n.vector, n.vector.miss)
        n.miss <- c(n.miss, n.miss.miss)
        a.list[[length(levels(group)) + 2]] <- a.list.miss
        ab <- matrix(NA, nrow = 6, ncol = (length(levels(group)) + 2))
      }

      if ("n" %in% n.or.miss)
        ab[1, ] <- n.vector
      if (!("n" %in% n.or.miss) & "miss" %in% n.or.miss)
        ab[1, ] <- n.miss
      for (d in 1:length(a.list)) {
        if (length(a.list[[d]]) != 0) {
          ab[2, d] <- formatr(mean(a.list[[d]]), digits.m)
          ab[3, d] <- formatr(sd(a.list[[d]]), digits.sd)
        }else{
          ab[2,d] <- "-"
          ab[3,d] <- "-"
        }
      }
      ab[4, ] <- sapply(a.list, med.new, simplify = T, k = digits.qu)
      ab[5, ] <- sapply(a.list, inqur, simplify = T, k = digits.qu)
      ab[6, ] <- sapply(a.list, minmax, simplify = T, k = digits.minmax)

      if ("n" %in% n.or.miss & "miss" %in% n.or.miss)
        ab <- rbind(ab[1, ], n.miss, ab[2:6, ])

      row.names(ab) <- NULL

      if (group.miss) {
        ab <- rbind(rep("", (length(levels(group)) + 2)), ab, rep("", (length(levels(group)) + 2)))
      } else {
        ab <- rbind(rep("", (length(levels(group)) + 1)), ab, rep("", (length(levels(group)) + 1)))
      }

      if (!(missing(var.names))) {
        if (data.names == T) {
          var.n1 <- var.names[i]
          var.n2 <- paste(paste("(", names(dat)[i], sep = ""), ")", sep = "")
          var.n <- paste(var.n1, var.n2, sep = " ")
        } else {
          var.n <- var.names[i]
        }
      } else {
        var.n <- names(dat)[i]
      }

      if (create == "word") {
        row.ab <- c()
        if ("n" %in% n.or.miss)
          row.ab <- c(row.ab, "    N")
        if ("miss" %in% n.or.miss)
            row.ab <- c(row.ab, "    Missing")
        if (!("n" %in% n.or.miss) & !("miss" %in% n.or.miss))
          row.ab <- c(row.ab, "  ")
        row.ab <- c(row.ab, "    Mean", "    SD", "    Median", "    Q1 -- Q3", "    Min. -- Max.")
      } else if (create == "R"){
        row.ab <- c()
        if ("n" %in% n.or.miss)
          row.ab <- c(row.ab, "  - N")
        if ("miss" %in% n.or.miss)
          row.ab <- c(row.ab, "   - Missing")
        if (!("n" %in% n.or.miss) & !("miss" %in% n.or.miss))
          row.ab <- c(row.ab, "  ")
        row.ab <- c(row.ab, "    - Mean", "    - SD", "    - Median", "    - Q1 -- Q3", "    - Min. -- Max.")
      } else {
        row.ab <- c()
        if ("n" %in% n.or.miss)
          row.ab <- c(row.ab, "    \\hspace{2ex} N ")
        if ("miss" %in% n.or.miss)
          row.ab <- c(row.ab, "\\hspace{2ex} Missing")
        if (!("n" %in% n.or.miss) & ! ("miss" %in% n.or.miss))
          row.ab <- c(row.ab, "  ")
        row.ab <- c(row.ab, "\\hspace{2ex} Mean", "\\hspace{2ex} SD",  "\\hspace{2ex} Median",
                    "\\hspace{2ex} Q1 -- Q3", "\\hspace{2ex} Min. -- Max.")
      }

      ab <- as.data.frame(ab)
      ab <- cbind(NA, ab)
      ab[, 1] <- c(var.n, row.ab, "")

      pvalues_var <- matrix(F, ncol = ncol(dat))
      pvalues_var[i] <- p.values
      if (pvalues_var[i]) {
        for (l in 1:length(levels(group))) {
          if (n.vector[ -length(n.vector)][l] < groupsize)
            pvalues_var[i] <- F
        }
        if (length(table(dat[,i])) == 1) {
          pvalues_var[i] = F
        } else {
          for (l in 1:length(table(dat[,i]))) {
            if (table(dat[,i])[l] == 0)
              pvalues_var[i] = F
          }
        }

      if (pvalues_var[i]) {
        if (index) {
          m <- m.cont(group, paired = paired, is.ordered = is.ordered(dat[, i]),
                      nonparametric = nonparametric[i], t.log = t.log[i])
          if (!(m %in% testings)) {
            testings <- c(testings, m)
            l.i <- l.i + 1
            index.i <- letters[l.i]
          }
        } else {
          index.i <- c()
        }
        pv <- p.cont(dat[ ,i], group, paired = paired, is.ordered = is.ordered(dat[ ,i]),
                     nonparametric = nonparametric[i], t.log = t.log[i], var.equal = var.equal,
                     index = index.i, create = create)
        index_var[i] <- T
      } else {
        index_var[i] <- F
        pv <- "--"
      }
        if (!("miss" %in% n.or.miss) & !("n" %in% n.or.miss)) {
          ab <- cbind(ab, c("","", pv, rep("", 5 + ("n" %in% n.or.miss & "miss" %in% n.or.miss))))
          ab <- ab[-2, ]
        } else {
          ab <- cbind(ab, c("", pv, rep("", 6 + ("n" %in% n.or.miss & "miss" %in% n.or.miss))))
        }
    }

      pos.i.alt <- pos.i

      pos.i <- pos.i + 8 + ("n" %in% n.or.miss & "miss" %in% n.or.miss)
    }
    names(ab) <- 1:length(ab)

    if (i == 1) {
      ab1 <- ab
    } else {
      ab1 <- rbind(ab1, ab)
    }
    if (!silent)
      print(list("i" = i, "pos.i" = pos.i, "ab1" = ab1))
    if (landscape == F) {
      if (is.null(pos.pagebr))
        pos.pagebr <- 45
    } else {
      if (is.null(pos.pagebr)) {
        pos.pagebr <- 30
      }
    }
    if (pos.i > pos.pagebr * pos.mult & i <= ncol(dat)) {
      pos <- c(pos, pos.i.alt)
      pos.mult <- pos.mult + 1
    }
  }
  return(list("descr" = ab1, "pos" = pos, "pos.pagebr" = pos.pagebr, "testings" = testings, "pvalues_var" = pvalues_var))
}
