#' An implementation of Wu's test from Wu 2023 (doi: 10.1080/10543406.2022.2065500)
#'
#' @param ... Additional arguments passed on to methods.
#' @rdname wu.test
#' @export
wu.test <- function(...) UseMethod("wu.test")

#' @param x Can be either a `matrix`, `data.frame`, or `vector` of model predictions.
#' As a `matrix` or `data.frame`, `x` should be a \eqn{p*q} `matrix` of binary
#' predictions with \eqn{p} samples as rows and \eqn{q} models as columns. If
#' `x` is a `vector`, it should have length \eqn{p*q} and both `models` and
#' `samples` must be specified. The data must be able to be coerced to a `factor`
#' with two levels. Ignored if a formula is specified.
#' @param y If `x` is a `matrix` or `data.frame`, then `y` must be a `vector` of
#' length \eqn{p} indicating positive and negative cases. If `x` is a `vector`,
#' then `y` must be a `vector` of length \eqn{p*q}. The data must be able to be
#' coerced to a `factor` with the same two levels as `x`. Ignored if a formula is
#' specified.
#' @param models A `vector` of length \eqn{p*q} indicating which model the datum
#' corresponds to. This should have \eqn{q} levels each occurring exactly \eqn{p}
#' times. Ignored if `x` is a `matrix` or `data.frame` or a formula is specified.
#' @param samples A `vector` of length \eqn{p*q} indicating which subject the datum
#' corresponds to. This should have \eqn{p} levels each occurring exactly \eqn{q}
#' times. Ignored if `x` is a `matrix` or `data.frame` or a formula is specified.
#' @rdname wu.test
#' @method wu.test default
#' @exportS3Method jocomo::wu.test default
wu.test.default <- function(x, y, models, samples, ...) {
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
    y <- factor(y, ordered = T)
    if (nlevels(y) != 2L) {
        stop("'y' must have exactly 2 levels")
    }
    x <- structure(factor(x, levels = levels(y), ordered = T), dim = dim(x), class = c("matrix", "ordered", "factor"))
    if (any(is.na(x))) {
        stop("'x' must have the same levels as 'y'")
    }

    STATISTIC <- jocomo::wu.statistic(x = x, y = y)
    PARAMETER <- 2L * (dim(x)[2L] - 1L)
    PVAL <- stats::pchisq(STATISTIC, df = PARAMETER, lower.tail = FALSE)

    ## <FIXME split.matrix>
    names(STATISTIC) <- "Wu chi-squared"
    names(PARAMETER) <- "df"

    structure(list(statistic = STATISTIC,
                   parameter = PARAMETER,
                   p.value = PVAL,
                   method = "Wu's test",
                   data.name = DNAME), class = "htest")
}

#' Title
#'
#' @param formula A two-sided formula object describing predictions across
#' multiple models and samples. Formulas may be specified in either wide, long,
#' or cross-tabulated format. Refer to 'Details' for more information regarding
#' formula specification.
#' @param data an optional data frame containing the variables named in formula.
#' By default the variables are taken from the environment from which `wu.test` is
#' called. While data is optional, the package authors strongly recommend its use.
#' @rdname wu.test
#' @method wu.test formula
#' @details Three formats are availabe for formula specification.
#'
#' Wide format: The term on the left of the ~ operator should
#' refer to a factor with two levels indicating the true labels for each subject.
#' Terms should be separated by + operators and refer to predictions from each model.
#'
#' Long format: The term on the left of the ~ operator should
#' refer to a factor with two levels indicating the true labels for each subject.
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
#' @exportS3Method jocomo::wu.test formula
wu.test.formula <- function(formula, data = parent.frame(), ...) {
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

        return(.wu.test.formula.long(formula = formula, data = data, ...))
    } else if (xtabs.formula) {
        return(.wu.test.formula.xtabs(formula = formula, data = data, ...))
    } else {
        # Is it save to assume this is wide format?  I'm not sure, but for now I will ...
        return(.wu.test.formula.wide(formula = formula, data = data, ...))
    }
}

.wu.test.formula.long <- function(formula, data, ...) {
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

    ret <- jocomo::wu.test(x = x, y = y, models = models, samples = samples, ...)
    ret$data.name <- DNAME
    ret
}

.wu.test.formula.wide <- function(formula, data, ...) {
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

    ret <- wu.test(x = x, y = y, ...)
    ret$data.name <- DNAME
    ret
}

.wu.test.formula.xtabs <- function(formula, data, ...) {
    tmp <- formula[[3L]][[2L]]
    formula[[3L]][[2L]] <- formula[[3L]][[3L]]
    formula[[3L]][[3L]] <- tmp
    formula[[3L]][[1L]] <- as.name("+")

    xt <- stats::xtabs(formula = formula, data = data)

    tf <- stats::terms(formula, data = data)
    mf <- stats::model.frame(tf, data = data)

    nmf <- names(mf)
    DNAME <- paste0(paste0(names(mf)[1L:(length(nmf) - 1L)], collapse = ", "), ", and ", nmf[length(nmf)])

    ret <- jocomo::wu.test(xt)
    ret$data.name <- DNAME
    ret
}

#' Title
#'
#' @param xt An `xtabs` object of 3 or more dimensions indicating the
#' cross-tabulation of model predictions and true labels. Each factor must have
#' exactly two levels. The first dimension should refer to the true labels
#' while the remaining dimensions refer to the model predictions.
#' @rdname wu.test
#' @method wu.test xtabs
#' @exportS3Method jocomo::wu.test xtabs
wu.test.xtabs <- function(xt, ...) {
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

    STATISTIC <- jocomo::wu.statistic(xt = xt, ...)
    PARAMETER <- 2L * (length(dim(xt)) - 2L)
    PVAL <- stats::pchisq(STATISTIC, df = PARAMETER, lower.tail = FALSE)

    ## <FIXME split.matrix>
    names(STATISTIC) <- "Wu chi-squared"
    names(PARAMETER) <- "df"

    structure(list(statistic = STATISTIC, parameter = PARAMETER, p.value = PVAL, method = "Wu's test", data.name = DNAME), class = "htest")
}

#' An implementation of the extended McNemar statistic from Wu 2023 (doi: 10.1080/10543406.2022.2065500)
#'
#' @param ... Additional arguments passed on to methods.
#' @rdname wu.test
#' @export
wu.statistic <- function(...) UseMethod("wu.statistic")

#' Title
#'
#' @inheritParams wu.statistic
#' @param x An object which can be coerced to a `matrix` of size \eqn{p*q} where
#' \eqn{p} is the number of samples and \eqn{q} is the number of models.
#' The data should consist of two ordered levels and is coerced into a logical `matrix`.
#' @param y A `vector` of length \eqn{p} where
#' The data should consist of two ordered levels and is coerced into a logical `vector`.
#' @param correct Add 0.5 to each cell of the 2x2 contingency table to adjust for 0 counts
#' @export
wu.statistic.default <- function(x, y, correct = F, ...) {
    x <- structure(x == max(x), dim = dim(x), class = c("matrix", "logical"))
    y <- y == max(y)

    p <- dim(x)[1L]
    q <- dim(x)[2L]

    x.pred.pos.cases <- x[y, ]
    x.pred.neg.cases <- x[!y, ]


    k <- 1L
    n01 <- c()
    n10 <- c()
    m01 <- c()
    m10 <- c()

    p1 <- x.pred.pos.cases[, 1L] |>
        factor(levels = c(F, T))
    n1 <- x.pred.neg.cases[, 1L] |>
        factor(levels = c(F, T))
    for (i in 2L:q) {
        p2 <- x.pred.pos.cases[, i] |>
            factor(levels = c(F, T))
        n2 <- x.pred.neg.cases[, i] |>
            factor(levels = c(F, T))
        for (j in 2L:q) {
            p3 <- x.pred.pos.cases[, j] |>
                factor(levels = c(F, T))
            n3 <- x.pred.neg.cases[, j] |>
                factor(levels = c(F, T))

            tb.pos <- table(p1, p2, p3) + ifelse(correct, 0.5, 0)
            tb.neg <- table(n1, n2, n3) + ifelse(correct, 0.5, 0)

            n01[k] <- tb.pos[1L, 2L, 2L]
            n10[k] <- tb.pos[2L, 1L, 1L]
            m01[k] <- tb.neg[1L, 2L, 2L]
            m10[k] <- tb.neg[2L, 1L, 1L]

            k <- k + 1L
        }
    }

    N01 <- matrix(n01, nrow = q - 1L)
    N10 <- matrix(n10, nrow = q - 1L)
    M01 <- matrix(m01, nrow = q - 1L)
    M10 <- matrix(m10, nrow = q - 1L)

    a <- diag(N10) - diag(N01)
    A <- N10 + N01

    b <- diag(M10) - diag(M01)
    B <- M10 + M01

    as.numeric(t(a) %*% solve(A) %*% a + t(b) %*% solve(B) %*% b)
}


#' Title
#'
#' @inheritParams wu.statistic
#' @param xt An `xtabs` object of 3 or more dimensions indicating the
#' cross-tabulation of model predictions and true labels. Each factor must have
#' exactly two levels. The first dimension should refer to the true labels
#' while the remaining dimensions refer to the model predictions.
#' @param correct Add 0.5 to each cell of the 2x2 contingency table to adjust for 0 counts
#' @export
wu.statistic.xtabs <- function(xt, correct = F, ...) {
    if (any(dim(xt) != 2L)) {
        stop("All factors of xt must have exactly 2 levels.")
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

    k <- 1L
    n01 <- c()
    n10 <- c()
    m01 <- c()
    m10 <- c()

    for (i in 2L:q) {
        for (j in 2L:q) {
            sub.xt <- stats::xtabs(freq ~ y + X[, 1L] + X[, i] + X[, j])

            n01[k] <- sub.xt[1L, 1L, 2L, 2L] + ifelse(correct, 0.5, 0)
            n10[k] <- sub.xt[1L, 2L, 1L, 1L] + ifelse(correct, 0.5, 0)
            m01[k] <- sub.xt[2L, 1L, 2L, 2L] + ifelse(correct, 0.5, 0)
            m10[k] <- sub.xt[2L, 2L, 1L, 1L] + ifelse(correct, 0.5, 0)

            k <- k + 1L
        }
    }

    N01 <- matrix(n01, nrow = q - 1L)
    N10 <- matrix(n10, nrow = q - 1L)
    M01 <- matrix(m01, nrow = q - 1L)
    M10 <- matrix(m10, nrow = q - 1L)

    a <- diag(N10) - diag(N01)
    A <- N10 + N01

    b <- diag(M10) - diag(M01)
    B <- M10 + M01

    as.numeric(t(a) %*% solve(A) %*% a + t(b) %*% solve(B) %*% b)
}

coronary.disease.tabulated <- data.frame(
  Freq = c(215L, 571L, 9L, 20L, 31L, 152L, 1L, 24L, 22L, 47L, 13L, 33L, 16L, 160L, 25L, 126L),
  expand.grid(T3 = factor(c(1L, 0L), levels = c(0L, 1L)),
              T2 = factor(c(1L, 0L), levels = c(0L, 1L)),
              T1 = factor(c(1L, 0L), levels = c(0L, 1L)),
              D =  factor(c(1L, 0L), levels = c(0L, 1L))))

rownames(coronary.disease.tabulated) <- NULL

coronary.disease.wide <- do.call(rbind, apply(coronary.disease.tabulated, 1L, \(r) {
    data.frame(D = rep(r["D"], r["Freq"]),
               T1 = rep(r["T1"], r["Freq"]),
               T2 = rep(r["T2"], r["Freq"]),
               T3 = rep(r["T3"], r["Freq"]))
}))
rownames(coronary.disease.wide) <- NULL

coronary.disease.long <- cbind(coronary.disease.wide,
                               sample = factor(1L:dim(coronary.disease.wide)[1L])) |>
 reshape(idvar = c('sample', 'D'),
         direction = 'long',
         varying = c('T1', 'T2', 'T3'),
         v.names='x',
         timevar='model')
rownames(coronary.disease.long) <- NULL
