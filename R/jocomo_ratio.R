jocomo.ratio.test <- function(...) UseMethod("jocomo.ratio.test")

#' Title
#'
#' @param x TODO
#' @param y TODO
#' @param samples TODO
#' @param models TODO
#' @param groups TODO
#'
#' @return TODO
#' @export
#' @examples
#' 'TODO'
jocomo.ratio.test.default <- function(x, y, samples, models, groups) {

    if (is.matrix(x) || is.data.frame(x)) {

        DNAME <- paste0(deparse(substitute(x)), ", ",
                        deparse(substitute(y)), ", and ",
                        deparse(substitute(groups)))
        #TODO

    }else{
        DNAME <- paste(deparse(substitute(x)),
                          ", ",
                          deparse(substitute(y)),
                          ", ",
                          deparse(substitute(models)),
                          ", ",
                          deparse(substitute(samples)),
                          ", and ",
                          deparse(substitute(groups)),
                       sep = "")

        between.group.chisq <- jocomo.chisq.test(x = x,
                                                 y = y,
                                                 samples = samples,
                                                 groups = groups)

        within.group.chisq <- jocomo.chisq.test(x = x,
                                                y = y,
                                                samples = groups,
                                                groups = samples)
    }


  .combine.chisq(between.group.chisq, within.group.chisq, DNAME)

}

#' Title
#'
#' @param formula TODO
#' @param data TODO
#' @return TODO
#' @export
#' @examples
#' 'TODO'
jocomo.ratio.test.formula <- function(formula, data=parent.frame()) {}

.jocomo.ratio.test.formula.long <- function(formula, data){}

.jocomo.ratio.test.formula.wide <- function(formula, data){}

.jocomo.ratio.test.formula.xtabs <- function(formula, data){}

.combine.chisq <- function(between.groups, within.groups, data.name){
  # Used to combine the two "directions" of jocomo.chisq into a ratio statistic
  X1 <- between.groups[['statistic']]
  DF1 <- between.groups[['parameter']]

  X2 <- within.groups[['statistic']]
  DF2 <- within.groups[['parameter']]

  STATISTIC <- (X1 * DF2) / (X2 * DF1)
  PVAL <- pf(STATISTIC, DF1, DF2, lower.tail = FALSE)
  PARAMETER <- c(df1 = DF1, df2 = DF2)
  names(STATISTIC) <- "F-statistic"
  names(PARAMETER) <- c("df1", "df2")

  structure(list(statistic = STATISTIC,
                 parameter = PARAMETER,
                 p.value = PVAL,
                 data.name = data.name,
                 method = "Ratio Test for the Joint Comparison of Multiple Models"),
            class='htest')
}

#' Title
#'
#' @param xt TODO
#' @param data TODO
#' @return TODO
#' @export
#' @examples
#' 'TODO'
jocomo.ratio.test.xtabs <- function(xt, data=parent.frame()) {}

