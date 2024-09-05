jocomo.chisq.test <- function(...) UseMethod("jocomo.chisq.test")

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
jocomo.chisq.test.default <- function(x, y, samples, models, groups) {



    if (is.matrix(x) || is.data.frame(x)) {
        DNAME <- paste0(deparse(substitute(x)), ", ",
                        deparse(substitute(y)), ", and ",
                        deparse(substitute(groups)))

        if (!is.matrix(x)) {
            x <- as.matrix(x)
        }

        models <- factor(c(col(x)))
        samples <- factor(c(row(x)))

        if (any(diff(c(length(y), dim(x)[1L]))) != 0L) {
            stop("Number of rows in x does not match length of y")
        }


        # Calculate the test statistic for each fold
        groups <- as.factor(groups)
        s <- 1L:length(y)
        wu.stats <- stats::aggregate(s~groups, data=parent.frame(), FUN=\(idx, ...){
          jocomo::multiclass.wu.test(x = x[idx,],
                  y = y[idx])['statistic']
        })
    } else {
        # Make sure none of the subject/model entries are NA
        if (anyNA(samples) || anyNA(models)) {
            stop("NA's are not allowed in 'models' or 'samples'")
        }
        # Make sure everything is the same length
        if (any(diff(c(length(y), length(models), length(samples), length(x), length(groups))) != 0L)) {
            stop("'x', 'y', 'models', 'samples', and 'groups' must have the same length")
        }
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

        # Make sure we have complete data
        # Aggregate the models by groups & samples
        # For each combination of fold+subject, there should be the same number
        # of models -- if this is different for any of the strata, throw an error
        if (any(diff(stats::aggregate(models~groups+samples,
                               \(.x) unique(length(.x)),
                               data=parent.frame())$models) != 0L)){
             stop("There must be exactly one prediction from each model for each subject within a fold.")
        }

        # There should be the same number of samples for each model in each fold.
        # Unsure if this second check is actually necessary ...
        if (any(stats::aggregate(samples~groups,
                          data=stats::aggregate(samples~models+groups,
                                         FUN=\(.x) .x |> unique() |> length()),
                          FUN=\(.y) any(diff(.y) != 0L))$samples)){
             stop("There must be exactly one prediction from each model for each subject within a fold.")
        }

        # Make sure y is consistent for each subject
        if (any(sapply(split(c(y), samples), \(.x) length(unique(.x))) != 1L)) {
            stop("'y' must not differ within levels of 'samples'")
        }

        # Calculate the test statistic for each fold
        groups <- as.factor(groups)
        s <- 1L:length(y)
        wu.stats <- stats::aggregate(result~groups, data=data.frame(result=s, groups=groups), FUN=\(idx, ...){
            mc.wu <- jocomo::multiclass.wu.test(x = x[idx],
                  y = y[idx],
                  samples = factor(samples[idx]),
                  models = factor(models[idx]))
            stat <- mc.wu[['statistic']] |> as.numeric()
            df <- mc.wu[['parameter']] |> as.numeric()
            c(statistic=stat, parameter=df)
        })
    }

    #return(wu.stats)
    #j <- as.numeric(nlevels(as.factor(groups)))
    #k <- as.numeric(nlevels(as.factor(models)))
    STATISTIC <- sum(wu.stats[['result']][,'statistic'])#as.numeric(sum(wu.stats['s']))
    PARAMETER <- sum(wu.stats[['result']][,'parameter'])#nlevels(y) * j * (k - 1L)
    PVAL <- stats::pchisq(STATISTIC, df = PARAMETER, lower.tail = FALSE)

    ## <FIXME split.matrix>
    names(STATISTIC) <- "chi-squared"
    names(PARAMETER) <- "df"

    structure(list(statistic = STATISTIC,
                   parameter = PARAMETER,
                   p.value = PVAL,
                   method = "Chi-squared Test for the Joint Comparison of Multiple Models",
                   data.name = DNAME), class = "htest")
}

#' Title
#'
#' @param formula TODO
#' @param data TODO
#' @return TODO
#' @export
#' @examples
#' 'TODO'
jocomo.chisq.test.formula <- function(formula, data=parent.frame()) {}

.jocomo.chisq.test.formula.long <- function(formula, data){}

.jocomo.chisq.test.formula.wide <- function(formula, data){}

.jocomo.chisq.test.formula.xtabs <- function(formula, data){}

#' Title
#'
#' @param xt TODO
#' @param data TODO
#' @return TODO
#' @export
#' @examples
#' 'TODO'
jocomo.chisq.test.xtabs <- function(xt, data=parent.frame()) {}


