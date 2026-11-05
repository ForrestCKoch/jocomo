.multiclass_wu_validate_correct <- function(correct) {
    if (length(correct) != 1L || !is.logical(correct) || is.na(correct)) {
        stop("'correct' must be a single non-missing logical value")
    }
}

.multiclass_wu_validate_data <- function(x, y, correct = FALSE) {
    .multiclass_wu_validate_correct(correct)

    if (length(dim(x)) != 2L) {
        stop("'x' must be a two-dimensional object")
    }
    if (length(y) != nrow(x)) {
        stop("'y' must have one value for each row of 'x'")
    }
    if (ncol(x) < 2L) {
        stop("'x' must contain predictions from at least 2 models")
    }
    if (anyNA(x) || anyNA(y)) {
        stop("'x' and 'y' must not contain missing values")
    }

    y <- factor(y)
    if (nlevels(y) < 2L) {
        stop("'y' must have at least 2 levels")
    }

    list(x = x, y = y, q = ncol(x))
}

.multiclass_wu_class_counts <- function(x, y, level, weights = rep(1, nrow(x)),
                                        correct = FALSE) {
    .multiclass_wu_validate_correct(correct)

    if (length(weights) != nrow(x) || any(!is.finite(weights)) ||
        any(weights < 0)) {
        stop("'weights' must be finite, non-negative, and match the rows of 'x'")
    }

    row_idx <- y == level
    q <- ncol(x)
    n_pairs <- (q - 1L)^2L
    type_i <- numeric(n_pairs)
    type_ii <- numeric(n_pairs)
    k <- 1L

    for (i in 2L:q) {
        for (j in 2L:q) {
            m1 <- factor(x[row_idx, 1L] == level, levels = c(TRUE, FALSE))
            m2 <- factor(x[row_idx, i] == level, levels = c(TRUE, FALSE))
            m3 <- factor(x[row_idx, j] == level, levels = c(TRUE, FALSE))
            counts <- stats::xtabs(
                weights[row_idx] ~ m1 + m2 + m3
            )

            correction <- if (isTRUE(correct)) 0.5 else 0
            type_i[k] <- counts[1L, 2L, 2L] + correction
            type_ii[k] <- counts[2L, 1L, 1L] + correction
            k <- k + 1L
        }
    }

    list(type_i = type_i, type_ii = type_ii)
}

.multiclass_wu_class_statistic <- function(type_i, type_ii, q, level) {
    expected <- (q - 1L)^2L
    if (length(type_i) != expected || length(type_ii) != expected) {
        stop("Invalid multiclass Wu discordance counts")
    }

    type_i_matrix <- matrix(type_i, nrow = q - 1L)
    type_ii_matrix <- matrix(type_ii, nrow = q - 1L)
    differences <- diag(type_i_matrix) - diag(type_ii_matrix)
    covariance <- type_i_matrix + type_ii_matrix

    if (qr(covariance)$rank < nrow(covariance)) {
        stop(
            "The covariance matrix is singular for class '", level,
            "'; use correct = TRUE or provide discordant observations"
        )
    }

    as.numeric(
        t(differences) %*% solve(covariance) %*% differences
    )
}

.multiclass_wu_statistic_from_data <- function(x, y, weights = NULL,
                                                correct = FALSE) {
    validated <- .multiclass_wu_validate_data(x, y, correct)
    x <- validated$x
    y <- validated$y
    q <- validated$q
    if (is.null(weights)) {
        weights <- rep(1, nrow(x))
    }

    sum(vapply(levels(y), function(level) {
        counts <- .multiclass_wu_class_counts(
            x = x,
            y = y,
            level = level,
            weights = weights,
            correct = correct
        )
        .multiclass_wu_class_statistic(
            type_i = counts$type_i,
            type_ii = counts$type_ii,
            q = q,
            level = level
        )
    }, numeric(1L)))
}
