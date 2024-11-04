#' An extended version of Wu's Test allowing for multiclasses
#'
#' @param ... Additional arguments passed on to methods.
#' @rdname multiclass.wu.test
#' @export
multiclass.wu.test <- function(...) UseMethod("multiclass.wu.test")

#' Title
#'
#' @param x Can be either a `matrix`, `data.frame`, or `vector` of model predictions.
#' As a `matrix` or `data.frame`, `x` should be a \eqn{p*q} `matrix` of binary
#' predictions with \eqn{p} samples as rows and \eqn{q} models as columns. If
#' `x` is a `vector`, it should have length \eqn{p*q} and both `models` and
#' `samples` must be specified. The data must be able to be coerced to a `factor`
#' with two or more levels. Ignored if a formula is specified.
#' @param y If `x` is a `matrix` or `data.frame`, then `y` must be a `vector` of
#' length \eqn{p} indicating positive and negative cases. If `x` is a `vector`,
#' then `y` must be a `vector` of length \eqn{p*q}. The data must be able to be
#' coerced to a `factor` with the same levels as `x`. Ignored if a formula is
#' specified.
#' @param models A `vector` of length \eqn{p*q} indicating which model the datum
#' corresponds to. This should have \eqn{q} levels each occurring exactly \eqn{p}
#' times. Ignored if `x` is a `matrix` or `data.frame` or a formula is specified.
#' @param samples A `vector` of length \eqn{p*q} indicating which subject the datum
#' corresponds to. This should have \eqn{p} levels each occurring exactly \eqn{q}
#' times. Ignored if `x` is a `matrix` or `data.frame` or a formula is specified.
#' @rdname multiclass.wu.test
#' @method multiclass.wu.test default
#' @exportS3Method jocomo::multiclass.wu.test default
multiclass.wu.test.default <- function(x, y, models, samples, correct = F, warn = getOption("warn"), ...) {
    # The following code is adapted from stats::friedman.test

    # If x is a matrix then we don't need to do much
    if (is.matrix(x) || is.data.frame(x)) {
        DNAME <- paste0(deparse(substitute(x)), " and ", deparse(substitute(y)))

        if (!is.matrix(x)) {
            x <- as.matrix(x)
        }

        models <- factor(c(col(x)))
        samples <- factor(c(row(x)))

        if (any(diff(c(length(y), dim(x)[1L]))) != 0L) {
            stop("Number of rows in x does not match length of y")
        }

        k <- nlevels(models)
    } else {
        # Make sure none of the subject/model entries are NA
        if (anyNA(samples) || anyNA(models)) {
            stop("NA's are not allowed in 'models' or 'samples'")
        }
        # Make sure everything is the same length
        if (any(diff(c(length(y), length(models), length(samples), length(x))) != 0L)) {
            stop("'x', 'y', 'models' and 'samples' must have the same length")
        }
        DNAME <- paste(deparse(substitute(x)),
                          ", ",
                          deparse(substitute(y)),
                          ", ",
                          deparse(substitute(models)),
                          ", and ",
                          deparse(substitute(samples)),
                       sep = "")
        # Make sure we have complete data
        if (any(c(table(models, samples)) == 0L)) {
            stop("There must be exactly one prediction from each model for each subject.  At least one is missing")
        }
        if (any(c(table(models, samples)) > 1L)) {
            stop("There must be exactly one prediction from each model for each subject.  At least one is duplicated")
        }

        models <- factor(models)
        samples <- factor(samples)
        ## Need to ensure consistent order of observations within blocks.
        o <- order(models, samples)
        y <- y[o]
        x <- x[o]
        models <- models[o]
        samples <- samples[o]
        k <- nlevels(models)
        y <- matrix(unlist(split(c(y), samples)), ncol = k, byrow = TRUE)

        # Make sure y is consistent for each subject
        if (any(apply(y, 1L, \(s) length(unique(s)), simplify = T) != 1L)) {
            stop("'y' must not differ within levels of 'samples'")
        }
        # Remove unecessary data from y and convert to a vector ...
        y <- as.vector(y[, 1L])
    }

    ## <FIXME split.matrix> -- From friedman.test??
    x <- matrix(unlist(split(c(x), samples)), ncol = k, byrow = TRUE)
    to.keep <- stats::complete.cases(x) & stats::complete.cases(y)
    y <- y[to.keep]  # We only need the first column as it should not change
    x <- x[to.keep, ]

    # Convert everything to factors
    y <- factor(y)
    if (nlevels(y) < 2L) {
        stop("'y' must have 2 or more levels")
    }
    x <- structure(factor(x, levels = levels(y)), dim = dim(x), class = c("factor", "matrix", "array"))
    if (any(is.na(x)) & warn) {
        warning("'x' does not have the same levels as 'y'")
    }

    #STATISTIC <- jocomo::multiclass.wu.statistic(x = x, y = y)
    STATISTIC <- multiclass.wu.statistic(x = x, y = y, correct = correct)
    PARAMETER <- nlevels(y) * (dim(x)[2L] - 1L)
    PVAL <- stats::pchisq(STATISTIC, df = PARAMETER, lower.tail = FALSE)

    ## <FIXME split.matrix>
    names(STATISTIC) <- "chi-squared"
    names(PARAMETER) <- "df"

    structure(list(statistic = STATISTIC,
                   parameter = PARAMETER,
                   p.value = PVAL,
                   method = "Multiclass Wu's test",
                   data.name = DNAME), class = "htest")
}

#' Title
#'
#' @param formula A two-sided formula object describing predictions across
#' multiple models and samples. Formulas may be specified in either wide, long,
#' or cross-tabulated format. Refer to 'Details' for more information regarding
#' formula specification.
#' @param data an optional data frame containing the variables named in formula.
#' By default the variables are taken from the environment from which `multiclass.wu.test` is
#' called. While data is optional, the package authors strongly recommend its use.
#' @rdname multiclass.wu.test
#' @method multiclass.wu.test formula
#' @details Three formats are availabe for formula specification.
#'
#' Wide format: The term on the left of the ~ operator should
#' refer to a factor with two or more levels indicating the true labels for each subject.
#' Terms should be separated by + operators and refer to predictions from each model.
#'
#' Long format: The term on the left of the ~ operator should
#' refer to a factor with two or more levels indicating the true labels for each subject.
#' The formula should take the form `x:y|z` where `x` are the model predictions,
#' `y` describes the model which yielded the prediction, and `z` describes the
#' subject which the prediction is for.
#'
#' Cross-tabulated format: The term on the left of the ~ operator should refer to
#' the frequency, or total counts, for that stratum. Terms should be separated by + operators
#' and refer to predictions from each model. An additional term, separated from
#' the others by `||` should be included on the right hand side of the formula.
#' e.g with 3 models, x1, x2, and x3, and true labels y, the formula should follow
#' `Freq ~ x1 + x2 + x3 || y`
#' @exportS3Method jocomo::multiclass.wu.test formula
multiclass.wu.test.formula <- function(formula, data = parent.frame(), ...) {
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

    # This formula follows the long format specification if so, formula should be in the format of y~x:model|subject
    if (long.formula) {
        if ((typeof(formula[[3L]][[1L]]) != as.name("symbol")) || (typeof(formula[[3L]][[2L]]) != as.name("language")) || (length(formula[[3L]][[2L]]) !=
            3L) || (typeof(formula[[3L]][[3L]]) != as.name("symbol"))) {
            stop("Incorrect specification for 'formula'")
        }

        if ((formula[[3L]][[2L]][[1L]]) != as.name(":") || formula[[3L]][[1L]] != as.name("|")) {
            stop("Incorrect specification for 'formula'")
        }

        return(.multiclass.wu.test.formula.long(formula = formula, data = data, ...))
    } else if (xtabs.formula) {
        return(.multiclass.wu.test.formula.xtabs(formula = formula, data = data, ...))
    } else {
        # Is it save to assume this is wide format?  I'm not sure, but for now I will ...
        return(.multiclass.wu.test.formula.wide(formula = formula, data = data, ...))
    }
}

.multiclass.wu.test.formula.long <- function(formula, data, ...) {
    formula[[3L]][[1L]] <- as.name("+")
    formula[[3L]][[2L]][[1L]] <- as.name("+")


    tf <- stats::terms(formula, data = data)
    mf <- stats::model.frame(tf, data = data)

    if (dim(mf)[2L] != 4L) {
        stop("Incorrect specification for 'formula'")
    }

    nmf <- names(mf)
    DNAME <- paste0(paste0(names(mf)[1L:(length(nmf) - 1L)], collapse = ", "), ", and ", nmf[length(nmf)])

    y <- mf[, 1L]
    x <- mf[, 2L]
    models <- mf[, 3L]
    samples <- mf[, 4L]

    ret <- jocomo::multiclass.wu.test(x = x, y = y, models = models, samples = samples, ...)
    ret$data.name <- DNAME
    ret
}

.multiclass.wu.test.formula.wide <- function(formula, data, ...) {
    if (!is.null(data)) {
        tf <- stats::terms(formula, data = data)
        mf <- stats::model.frame(tf, data = data)
    } else {
        tf <- stats::terms(formula)
        mf <- stats::model.frame(tf)
    }

    nmf <- names(mf)
    if (length(nmf) == 2L) {
        DNAME <- paste0(nmf, collapse = " and ")
    } else {
        DNAME <- paste0(paste0(nmf[1L:(length(nmf) - 1L)], collapse = ", "), ", and ", nmf[length(nmf)])
    }

    y <- mf[, 1L]
    x <- mf[, -1L]

    ret <- jocomo::multiclass.wu.test(x = x, y = y, ...)
    ret$data.name <- DNAME
    ret
}

.multiclass.wu.test.formula.xtabs <- function(formula, data, ...) {
    tmp <- formula[[3L]][[2L]]
    formula[[3L]][[2L]] <- formula[[3L]][[3L]]
    formula[[3L]][[3L]] <- tmp
    formula[[3L]][[1L]] <- as.name("+")

    xt <- stats::xtabs(formula = formula, data = data)

    tf <- stats::terms(formula, data = data)
    mf <- stats::model.frame(tf, data = data)

    nmf <- names(mf)
    DNAME <- paste0(paste0(names(mf)[1L:(length(nmf) - 1L)], collapse = ", "), ", and ", nmf[length(nmf)])

    ret <- jocomo::multiclass.wu.test(xt)
    ret$data.name <- DNAME
    ret
}

#' Title
#'
#' @param xt An `xtabs` object of 3 or more dimensions indicating the
#' cross-tabulation of model predictions and true labels. Each factor must have
#' the same number of levels. The first dimension should refer to the true labels
#' while the remaining dimensions refer to the model predictions.
#' @rdname multiclass.wu.test
#' @method multiclass.wu.test xtabs
#' @exportS3Method jocomo::multiclass.wu.test xtabs
multiclass.wu.test.xtabs <- function(xt, ...) {
    if (!is.table(xt)) {
        stop("xt must be an xtabs or table object")
    }

    if (any(dim(xt) != 2L)) {
        stop("All factors of xt must have exactly 2 levels.")
    }

    k <- length(dim(xt)) - 1L

    if (k < 1L) {
        stop("xt has too few dimensions")
    }

    # If x is a matrix then we don't need to do much
    DNAME <- paste0(deparse(substitute(xt)))

    STATISTIC <- jocomo::multiclass.wu.statistic(xt = xt, ...)
    PARAMETER <- dim(xt)[1] * (length(dim(xt)) - 2L)
    PVAL <- stats::pchisq(STATISTIC, df = PARAMETER, lower.tail = FALSE)

    ## <FIXME split.matrix>
    names(STATISTIC) <- "chi-squared"
    names(PARAMETER) <- "df"

    structure(list(statistic = STATISTIC, parameter = PARAMETER, p.value = PVAL, method = "Multiclass Wu's test", data.name = DNAME), class = "htest")
}

#' An implementation of the extended McNemar statistic from Wu 2023 (doi: 10.1080/10543406.2022.2065500)
#'
#' @param ... Additional arguments passed on to methods.
#' @rdname multiclass.wu.test
#' @export
multiclass.wu.statistic <- function(...) UseMethod("multiclass.wu.statistic")

#' Title
#'
#' @inheritParams multiclass.wu.statistic
#' @param x An object which can be coerced to a `matrix` of size \eqn{p*q} where
#' \eqn{p} is the number of samples and \eqn{q} is the number of models.
#' The data should consist of two  or more levels.
#' @param y A `vector` of length \eqn{p} where
#' The data should consist of two or more levels.
#' @param correct Add 0.5 to each cell of the 2x2 contingency table to adjust for 0 counts
#' @export
multiclass.wu.statistic.default <- function(x, y, correct = F, ...) {
    #x <- structure(x == max(x), dim = dim(x), class = c("matrix", "logical"))
    #y <- y == max(y)

    p <- dim(x)[1L]
    q <- dim(x)[2L]

    statistic <- 0
    for(level in levels(y)){

      type.i <- c()
      type.ii <- c()
      k <- 1L

      row.idx <- which(y==level)

      m1 <- (x[row.idx, 1L] == level) |> factor(levels=c(T,F))

      for (i in 2L:q) {
        m2 <- (x[row.idx, i] == level) |> factor(levels=c(T,F))
        for (j in 2L:q) {
          m3 <- (x[row.idx, j] == level) |> factor(levels=c(T,F))

          tb <- table(m1, m2, m3) + ifelse(correct, stats::runif(1,1/1e4,5/1e4), 0)

          type.i[k] <- tb[1L, 2L, 2L]
          type.ii[k] <- tb[2L, 1L, 1L]

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


#' Title
#'
#' @inheritParams multiclass.wu.statistic
#' @param xt An `xtabs` object of 3 or more dimensions indicating the
#' cross-tabulation of model predictions and true labels. Each factor must have
#' exactly two levels. The first dimension should refer to the true labels
#' while the remaining dimensions refer to the model predictions.
#' @param correct Add 0.5 to each cell of the 2x2 contingency table to adjust for 0 counts
#' @export
multiclass.wu.statistic.xtabs <- function(xt, correct = F, ...) {
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
