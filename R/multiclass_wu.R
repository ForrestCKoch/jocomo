#' An extended version of Wu's Test allowing for more than 2 classes
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
#' with two or more levels. Predictions may include valid class levels that are
#' absent from `y` in the supplied subset. Missing predictions are not allowed.
#' Ignored if a formula is specified.
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
#' @param correct A boolean value indicating whether adjustments should be made
#' acount for empty cells in the contingency tables (required if any cells would be 0)
#' @param warn A boolean value indicating whether to issue warnings when data appears
#' incorretly structured.
#' @rdname multiclass.wu.test
#' @method multiclass.wu.test default
#' @exportS3Method jocomo::multiclass.wu.test default
multiclass.wu.test.default <- function(x, y, models, samples,
                                       correct = FALSE,
                                       warn = FALSE, ...) {
    # The following code is adapted from stats::friedman.test

    # If x is a matrix then we don't need to do much
    if (is.matrix(x) || is.data.frame(x)) {
        DNAME <- paste0(deparse(substitute(x)), " and ", deparse(substitute(y)))

        if (!is.matrix(x)) {
            x <- as.matrix(x)
        }

        models <- factor(c(col(x)))
        samples <- factor(c(row(x)))

        if (length(y) != nrow(x)) {
            stop("Number of rows in x does not match length of y")
        }
        if (anyNA(x) || anyNA(y)) {
            stop("'x' and 'y' must not contain missing values")
        }

        k <- nlevels(models)
        if (k < 2L) {
            stop("'x' must contain predictions from at least 2 models")
        }
    } else {
        DNAME <- paste(deparse(substitute(x)),
                          ", ",
                          deparse(substitute(y)),
                          ", ",
                          deparse(substitute(models)),
                          ", and ",
                          deparse(substitute(samples)),
                       sep = "")
        normalized <- .jocomo_normalize_long_data(x, y, models, samples)
        x <- normalized$x
        y <- normalized$y
        models <- normalized$models
        samples <- normalized$samples
        k <- normalized$k
    }

    ## <FIXME split.matrix> -- From friedman.test??
    x <- matrix(unlist(split(c(x), samples)), ncol = k, byrow = TRUE)
    to.keep <- rep(TRUE, nrow(x))
    y <- y[to.keep]  # We only need the first column as it should not change
    x <- x[to.keep, ]

    # Keep prediction labels outside the observed outcome classes: a model can
    # validly predict a class absent from the current sample.
    y <- factor(y)
    if (nlevels(y) < 2L) {
        stop("'y' must have at least 2 levels")
    }

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
#'
#' Prediction columns may contain classes that do not occur in the observed
#' outcome vector for a subset of the data. Such predictions are retained and
#' are not treated as missing values.
#' @exportS3Method jocomo::multiclass.wu.test formula
multiclass.wu.test.formula <- function(formula, data = parent.frame(), ...) {
    # Sanity checks
    if (missing(formula)) {
        stop("formula missing")
    }
    kind <- .jocomo_formula_kind(formula)
    switch(
        kind,
        long = .multiclass.wu.test.formula.long(formula = formula, data = data, ...),
        xtabs = .multiclass.wu.test.formula.xtabs(formula = formula, data = data, ...),
        wide = .multiclass.wu.test.formula.wide(formula = formula, data = data, ...)
    )
}

.multiclass.wu.test.formula.long <- function(formula, data, ...) {
    normalized_formula <- .jocomo_long_formula(formula)
    tf <- stats::terms(normalized_formula, data = data)
    mf <- .jocomo_model_frame(tf, data = data)

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
        mf <- .jocomo_model_frame(tf, data = data)
    } else {
        tf <- stats::terms(formula)
        mf <- .jocomo_model_frame(tf, data = data)
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
    normalized_formula <- .jocomo_xtabs_formula(formula)
    mf <- .jocomo_model_frame(normalized_formula, data = data)
    .jocomo_validate_frequency(mf)
    .jocomo_validate_xtabs_models(mf)
    xt <- stats::xtabs(formula = normalized_formula, data = data)

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

    if (length(unique(dim(xt))) != 1L) {
        stop("All factors of xt must have the same number of levels.")
    }

    k <- length(dim(xt)) - 1L

    if (k < 2L) {
        stop("xt must contain predictions from at least 2 models")
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
#' The data should consist of two or more levels. Prediction values may include
#' valid classes absent from `y`; missing values are not allowed.
#' @param y A `vector` of length \eqn{p} where
#' The data should consist of two or more levels.
#' @param correct Adds a small value to each cell of the 2x2 contingency table to adjust for 0 counts
#' @export
multiclass.wu.statistic.default <- function(x, y, correct = FALSE, ...) {
    .multiclass_wu_statistic_from_data(
        x = x,
        y = y,
        correct = correct
    )
}


#' Title
#'
#' @inheritParams multiclass.wu.statistic
#' @param xt An `xtabs` object of 3 or more dimensions indicating the
#' cross-tabulation of model predictions and true labels. Each factor must have
#' exactly two levels. The first dimension should refer to the true labels
#' while the remaining dimensions refer to the model predictions.
#' @param correct Adds a small value to each cell of the 2x2 contingency table to adjust for 0 counts
#' @export
multiclass.wu.statistic.xtabs <- function(xt, correct = FALSE, ...) {
    if (!is.table(xt)) {
        stop("'xt' must be a table or xtabs object")
    }
    if (length(unique(dim(xt))) != 1L) {
        stop("All factors of xt must have the same number of levels.")
    }

    q <- length(dim(xt)) - 1L
    if (q < 2L) {
        stop("xt must contain predictions from at least 2 models")
    }

    xt.df <- data.frame(xt)
    freq <- xt.df[["Freq"]]
    predictors <- xt.df[, -c(1L, ncol(xt.df)), drop = FALSE]
    outcome <- xt.df[[1L]]

    if (!is.numeric(freq) || any(!is.finite(freq)) || any(freq < 0)) {
        stop("xt frequencies must be finite, numeric, and non-negative")
    }

    .multiclass_wu_statistic_from_data(
        x = predictors,
        y = outcome,
        weights = freq,
        correct = correct
    )
}
