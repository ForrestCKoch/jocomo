#' Title
#'
#' @param ... Additional arguments passed on to methods.
#' @rdname jocomo.chisq.test
#' @export
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
#' @rdname jocomo.chisq.test
#' @examples
#' 'TODO'
#' @method jocomo.chisq.test default
#' @exportS3Method jocomo::jocomo.chisq.test default
jocomo.chisq.test.default <- function(x, y, samples, models, groups, correct = F, warn = getOption("warn"), ...) {



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
                  y = y[idx], correct = correct, warn = warn)['statistic']
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
        # TODO: is this needed?
        # if (any(diff(stats::aggregate(models~groups+samples,
        #                        \(.x) unique(length(.x)),
        #                        data=parent.frame())$models) != 0L)){
        #      stop("There must be exactly one prediction from each model for each subject within a fold.")
        # }

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
        wu.stats <- stats::aggregate(result~groups, #data=data.frame(result=s, groups=groups), FUN=\(idx, ...){
                                     data=data.frame(result=s, groups=groups), FUN=\(idx, ...){
            #mc.wu <- jocomo::multiclass.wu.test(x = x[idx],
            mc.wu <- multiclass.wu.test(x = x[idx],
                  y = y[idx],
                  samples = factor(samples[idx]),
                  models = factor(models[idx]),
                  correct = correct,
                  warn = warn)
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
#' @rdname jocomo.chisq.test
#' @examples
#' 'TODO'
#' @exportS3Method jocomo::jocomo.chisq.test formula
jocomo.chisq.test.formula <- function(formula, data=parent.frame(), ...) {
    stop("Not Implemented")
    # Sanity checks
    if (missing(formula)) {
        stop("formula missing")
    }
    if (length(formula) != 3L) {
        stop("incorrect specification for 'formula'")
    }

    long.formula <- F
    xtabs.formula <- F
    if (typeof(formula[[3L]]) == as.name("language")) {
        if (formula[[3L]][[1L]] != as.name("+")) {
            if (formula[[3L]][[1L]] == as.name("|")) {
                long.formula <- T
            }
            if (formula[[3L]][[1L]] == as.name("||")) {
                xtabs.formula <- T
            }
        }
    }

    # This formula follows the long format specification if so, formula should be in the format of y~x:model|subject|group
    if (long.formula) {
        if ((typeof(formula[[3L]][[1L]]) != as.name("symbol")) || (typeof(formula[[3L]][[2L]]) != as.name("language")) || (length(formula[[3L]][[2L]]) !=
            3L) || (typeof(formula[[3L]][[3L]]) != as.name("symbol"))) {
            stop("Incorrect specification for 'formula'")
        }

        if ((formula[[3L]][[2L]][[1L]]) != as.name("|") || formula[[3L]][[1L]] != as.name("|") ||
             length(formula[[3L]][[2L]][[2L]]) != 3) {
            stop("Incorrect specification for 'formula'")
        }

        if (length(formula[[3L]][[2L]][[2L]][[1L]]) != as.name(':')) {
            stop("Incorrect specification for 'formula'")
        }

        # More error checking should go here to check for correct format ...
        return(.jocomo.chisq.test.formula.long(formula = formula, data = data, ...))
    } else if (xtabs.formula) {

        # More error checking should go here to check for correct format ...
        return(.jocomo.chisq.test.formula.xtabs(formula = formula, data = data, ...))
    } else {
        # Is it save to assume this is wide format?  I'm not sure, but for now I will ...
        return(.jocomo.chisq.formula.wide(formula = formula, data = data, ...))
    }
}

.jocomo.chisq.test.formula.long <- function(formula, data, ...){
    stop("Not Implemented")
    formula[[3L]][[1L]] <- as.name("+")
    formula[[3L]][[2L]][[1L]] <- as.name("+")
    formula[[3L]][[2L]][[2L]][[1L]] <- as.name("+")


    tf <- stats::terms(formula, data = data)
    mf <- stats::model.frame(tf, data = data)

    if (dim(mf)[2L] != 5L) {
        stop("Incorrect specification for 'formula'")
    }

    nmf <- names(mf)
    DNAME <- paste0(paste0(names(mf)[1L:(length(nmf) - 1L)], collapse = ", "), ", and ", nmf[length(nmf)])

    y <- mf[, 1L]
    x <- mf[, 2L]
    models <- mf[, 3L]
    groups <- mf[, 4L]
    samples <- mf[, 5L]

    ret <- jocomo::jocomo.chisq.test(x = x,
                                     y = y,
                                     models = models,
                                     samples = samples,
                                     groups = groups, ...)
    ret$data.name <- DNAME
    ret
}

.jocomo.chisq.test.formula.wide <- function(formula, data, ...){
    stop("Not Implemented")
}

.jocomo.chisq.test.formula.xtabs <- function(formula, data, ...){
    stop("Not Implemented")

    # Rearrange things a bit, this is kinda hacky, but basically we are taking
    # x1+x2+...+xn||group||label to
    # label+(group + (x1+x2+...+xn))
    tmp <- formula[[3L]][[2L]][[2L]]
    formula[[3L]][[2L]][[2L]] <- formula[[3L]][[2L]][[3L]]
    formula[[3L]][[2L]][[3L]] <- tmp
    formula[[3L]][[2L]][[1L]] <- as.name("+")

    tmp <- formula[[3L]][[2L]]
    formula[[3L]][[2L]] <- formula[[3L]][[3L]]
    formula[[3L]][[3L]] <- tmp
    formula[[3L]][[1L]] <- as.name("+")


    xt <- stats::xtabs(formula = formula, data = data)

    tf <- stats::terms(formula, data = data)
    mf <- stats::model.frame(tf, data = data)

    nmf <- names(mf)
    DNAME <- paste0(paste0(names(mf)[1L:(length(nmf) - 1L)], collapse = ", "), ", and ", nmf[length(nmf)])

    ret <- jocomo::jocomo.chisq.test(xt)
    ret$data.name <- DNAME
    ret
}

#' Title
#'
#' @param xt TODO
#' @param data TODO
#' @return TODO
#' @rdname jocomo.chisq.test
#' @examples
#' 'TODO'
#' @exportS3Method jocomo::jocomo.chisq.test xtabs
jocomo.chisq.test.xtabs <- function(xt, data=parent.frame()) {
    stop("Not Implemented")
    if (any(diff(dim(xt)) != 0L)) {
        stop("All factors of xt must have the same number of levels.")
    }


    # p <- dim(x)[1]
    q <- length(dim(xt)) - 1L
    if (q < 1L) {
        stop("xt has too few dimensions")
    }

    # x.pred.pos.cases <- x[y,] x.pred.neg.cases <- x[!y,]

    xt.df <- data.frame(xt)
    freq <- xt.df[["Freq"]]
    Freq <- NULL  # Needed to remove notes, should be defined in xt.df ...
    X <- subset(xt.df, select = -Freq)[, -1]
    y <- subset(xt.df, select = -Freq)[, 1]

    statistic <- 0
    for(level in levels(y)){

      type.i <- c()
      type.ii <- c()
      k <- 1L

      row.idx <- which(y==level)

      m1 <- (X[row.idx, 1L] == level) |> factor(levels=c(T,F))
      for (i in 2L:q) {
        m2 <- (X[row.idx, i] == level) |> factor(levels=c(T,F))
        for (j in 2L:q) {
          m3 <- (X[row.idx, j] == level) |> factor(levels=c(T,F))
          sub.xt <- stats::xtabs(freq[row.idx] ~ m1 + m2 + m3)

          type.i[k] <- sub.xt[1L, 2L, 2L] + ifelse(correct, stats::runif(1,1/1e4,5/1e4), 0)# ifelse(correct, 0.0001, 0)
          type.ii[k] <- sub.xt[2L, 1L, 1L] + ifelse(correct, stats::runif(1,1/1e4,5/1e4), 0)# ifelse(correct, 0.0001, 0)

          k <- k + 1L
        }
      }
      type.i.matrix <- matrix(type.i, nrow = q - 1L)
      type.ii.matrix <- matrix(type.ii, nrow = q - 1L)

      a <- diag(type.i.matrix) - diag(type.ii.matrix)
      A <- type.i.matrix + type.ii.matrix

      statistic <- statistic + ( t(a) %*% solve(A) %*% a )
    }
  statistic
}


