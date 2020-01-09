#' DescrTab: A package to create descriptive statistics tables.
#'
#' The packages creates descriptive statistics such as mean, standard
#' deviation or relative and absolute frequencies grouped by a factor and also
#' provides p-values obtained from testing for differences between the groups.
#'
#' @section Want to learn?:
#' Type in vignette(package="DescrTab")
#'
#' @docType package
#' @name DescrTab
NULL




#' Data that is made to look like clinical data.
#'
#' For teaching purposes
#'
#' @docType data
#'
#' @format A data frame with 200 rows and 5 variables:
#' \describe{
#'   \itme{group}{control or treatment}
#'   \item{gender}{male or female}
#'   \item{age}{number between 15-90}
#'   \item{blood_hg}{Number between 50 and 150}
#'   \item{QoL_score}{sad, normal or happy}
#'   ...
#' }
"madeUpData"
