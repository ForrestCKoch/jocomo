jocomo.test <- function(...) UseMethod("jocomo.test")

#' Title
#'
#' @param x
#' @param y
#' @param subjects
#' @param models
#' @param folds
#'
#' @return
#' @export
#'
#' @examples
jocomo.test.default <- function(x, y, subjects, models, folds) {



    if (is.matrix(x) || is.data.frame(x)) {
        DNAME <- paste0(deparse(substitute(x)), ", ",
                        deparse(substitute(y)), ", and ",
                        deparse(substitute(folds)))

        if (!is.matrix(x)) {
            x <- as.matrix(x)
        }

        models <- factor(c(col(x)))
        subjects <- factor(c(row(x)))

        if (any(diff(c(length(y), dim(x)[1]))) != 0L) {
            stop("Number of rows in x does not match length of y")
        }


        # Calculate the test statistic for each fold
        folds <- as.factor(folds)
        s <- 1:length(y)
        wu.stats <- aggregate(s~folds, data=parent.frame(), FUN=\(idx, ...){
          wu.test(x = x[idx,],
                  y = y[idx])['statistic']
        })
    } else {
        # Make sure none of the subject/model entries are NA
        if (anyNA(subjects) || anyNA(models)) {
            stop("NA's are not allowed in 'models' or 'subjects'")
        }
        # Make sure everything is the same length
        if (any(diff(c(length(y), length(models), length(subjects), length(x), length(folds))) != 0L)) {
            stop("'x', 'y', 'models', 'subjects', and 'folds' must have the same length")
        }
        DNAME <- paste(deparse(substitute(x)),
                          ", ",
                          deparse(substitute(y)),
                          ", ",
                          deparse(substitute(models)),
                          ", ",
                          deparse(substitute(subjects)),
                          ", and ",
                          deparse(substitute(folds)),
                       sep = "")

        # Make sure we have complete data
        # Aggregate the models by folds & subjects
        # For each combination of fold+subject, there should be the same number
        # of models -- if this is different for any of the strata, throw an error
        if (any(diff(aggregate(models~folds+subjects,
                               \(.x) unique(length(.x)),
                               data=parent.frame())$models) != 0L)){
             stop("There must be exactly one prediction from each model for each subject within a fold.")
        }

        # There should be the same number of subjects for each model in each fold.
        # Unsure if this second check is actually necessary ...
        if (any(aggregate(subjects~folds,
                          data=aggregate(subjects~models+folds,
                                         FUN=\(.x) .x |> unique() |> length()),
                          FUN=\(.y) any(diff(.y) != 0L))$subjects)){
             stop("There must be exactly one prediction from each model for each subject within a fold.")
        }

        # Make sure y is consistent for each subject
        if (any(sapply(split(c(y), subjects), \(.x) length(unique(.x))) != 1L)) {
            stop("'y' must not differ within levels of 'subjects'")
        }

        # Calculate the test statistic for each fold
        folds <- as.factor(folds)
        s <- 1:length(y)
        wu.stats <- aggregate(s~folds, data=data.frame(s=s, folds=folds), FUN=\(idx, ...){
          as.numeric(
            wu.test(x = x[idx],
                  y = y[idx],
                  subjects = factor(subjects[idx]),
                  models = factor(models[idx]))['statistic'])
        })
    }

    #return(wu.stats)
    j <- as.numeric(nlevels(as.factor(folds)))
    k <- as.numeric(nlevels(as.factor(models)))
    STATISTIC <- as.numeric(sum(wu.stats['s']))
    PARAMETER <- 2 * j * (k - 1)
    PVAL <- stats::pchisq(STATISTIC, df = PARAMETER, lower.tail = FALSE)

    ## <FIXME split.matrix>
    names(STATISTIC) <- "chi-squared"
    names(PARAMETER) <- "df"

    structure(list(statistic = STATISTIC,
                   parameter = PARAMETER,
                   p.value = PVAL,
                   method = "Joint Comparison of Multiple Models",
                   data.name = DNAME), class = "htest")
}

#' Title
#'
#' @param formula
#' @param data
#'
#' @return
#' @export
#'
#' @examples
jocomo.test.formula <- function(formula, data=parent.frame()) {}

.jocomo.test.formula.long <- function(formula, data){}

.jocomo.test.formula.wide <- function(formula, data){}

.jocomo.test.formula.xtabs <- function(formula, data){}

#' Title
#'
#' @param xt
#' @param data
#'
#' @return
#' @export
#'
#' @examples
jocomo.test.xtabs <- function(xt, data=parent.frame()) {}


