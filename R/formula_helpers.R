.jocomo_formula_kind <- function(formula) {
    if (!inherits(formula, "formula") || length(formula) != 3L) {
        stop("formula must be a two-sided formula")
    }

    rhs <- formula[[3L]]
    if (!is.call(rhs)) {
        return("wide")
    }

    operator <- rhs[[1L]]
    if (identical(operator, as.name("|"))) {
        if (length(rhs) != 3L || !is.call(rhs[[2L]]) ||
            !identical(rhs[[2L]][[1L]], as.name(":")) ||
            length(rhs[[2L]]) != 3L) {
            stop("Long formulas must have the form response ~ prediction:model | sample")
        }
        return("long")
    }

    if (identical(operator, as.name("||"))) {
        if (length(rhs) != 3L) {
            stop("Cross-tabulated formulas must have the form Freq ~ predictions || outcome")
        }
        return("xtabs")
    }

    if (identical(operator, as.name("+"))) {
        return("wide")
    }

    stop("Unsupported formula RHS; use wide, long, or cross-tabulated syntax")
}

.jocomo_plus_terms <- function(expr) {
    if (is.call(expr) && identical(expr[[1L]], as.name("+"))) {
        c(.jocomo_plus_terms(expr[[2L]]), .jocomo_plus_terms(expr[[3L]]))
    } else {
        list(expr)
    }
}

.jocomo_formula_with_rhs <- function(formula, rhs) {
    stats::as.formula(
        call("~", formula[[2L]], rhs),
        env = environment(formula)
    )
}

.jocomo_model_frame <- function(formula, data) {
    arguments <- list(formula = formula, na.action = stats::na.pass)
    if (!is.null(data)) {
        arguments$data <- data
    }
    mf <- do.call(stats::model.frame, arguments)
    if (anyNA(mf)) {
        stop("formula data must not contain missing values")
    }
    mf
}

.jocomo_validate_frequency <- function(mf) {
    frequency <- mf[[1L]]
    if (!is.numeric(frequency) || any(!is.finite(frequency)) ||
        any(frequency < 0)) {
        stop("frequency must be finite, numeric, and non-negative")
    }
}

.jocomo_validate_xtabs_models <- function(mf) {
    n_models <- ncol(mf) - 2L
    if (n_models < 2L) {
        stop("Cross-tabulated formulas must contain at least 2 model predictions")
    }
}

.jocomo_long_formula <- function(formula) {
    rhs <- formula[[3L]]
    prediction <- rhs[[2L]][[2L]]
    model <- rhs[[2L]][[3L]]
    sample <- rhs[[3L]]
    normalized_rhs <- call(
        "+",
        call("+", prediction, model),
        sample
    )
    .jocomo_formula_with_rhs(formula, normalized_rhs)
}

.jocomo_xtabs_formula <- function(formula) {
    rhs <- formula[[3L]]
    predictions <- .jocomo_plus_terms(rhs[[2L]])
    if (length(predictions) < 2L &&
        !identical(predictions[[1L]], as.name("."))) {
        stop("Cross-tabulated formulas must contain at least 2 model predictions")
    }

    normalized_rhs <- Reduce(
        function(left, right) call("+", left, right),
        c(list(rhs[[3L]]), predictions)
    )
    .jocomo_formula_with_rhs(formula, normalized_rhs)
}
