#' An extended version of Wu's Test allowing for more than 2 classes or datasets.
#'
#' @param ... Additional arguments passed on to methods.
#' @rdname jocomo.chisq.test
#' @export
jocomo.chisq.test <- function(...) UseMethod("jocomo.chisq.test")

#' An extended version of Wu's Test allowing for more than 2 classes or datasets.
#'
#' @inheritParams jocomo.chisq.test
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
#' @param samples A `vector` of length \eqn{p*q} indicating which subject the datum
#' corresponds to. Ignored if `x` is a `matrix` or `data.frame` or a formula is specified.
#' @param models A `vector` of length \eqn{p*q} indicating which model the datum
#' corresponds to. Ignored if `x` is a `matrix` or `data.frame` or a formula is specified.
#' @param group A `vector` of length \eqn{p*q} indicating which strata the datum
#' corresponds to. Ignored if `x` is a `matrix` or `data.frame` or a formula is specified.
#' @param correct A boolean value indicating whether adjustments should be made
#' acount for empty cells in the contingency tables (required if any cells would be 0)
#' @param warn A boolean value indicating whether to issue warnings when data appears
#' incorretly structured.
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
        wu.stats <- stats::aggregate(result~groups, data=data.frame(result=s, groups=groups), FUN=\(idx, ...){
            mc.wu <- jocomo::multiclass.wu.test(x = x[idx,],
                  y = y[idx], correct = correct, warn = warn)
            stat <- mc.wu[['statistic']] |> as.numeric()
            df <- mc.wu[['parameter']] |> as.numeric()
            c(statistic=stat, parameter=df)
        })['result']
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
    # Sanity checks
    if (missing(formula)) {
        stop("formula missing")
    }
    if (length(formula) != 3L) {
        stop("incorrect specification for 'formula'")
    }

    df.formula <- F
    xtabs.formula <- F
    long.formula <- F
    wide.formula <- F
    if (typeof(formula[[3L]]) == as.name("language")) {
        if (formula[[3L]][[1L]] != as.name("+")) {
            if (formula[[3L]][[1L]] == as.name("|")) {
                df.formula <- T
            }
            if (formula[[3L]][[1L]] == as.name("||")) {
                xtabs.formula <- T
            }
        }
    }

    # This formula follows the long format specification if so, formula should be in the format of y~x:model|subject|group
    if (df.formula) {
        if ((typeof(formula[[3L]][[1L]]) != as.name("symbol")) || (typeof(formula[[3L]][[2L]]) != as.name("language")) || (length(formula[[3L]][[2L]]) !=
            3L) || (typeof(formula[[3L]][[3L]]) != as.name("symbol"))) {
            stop("Incorrect specification for 'formula'")
        }

        if ((formula[[3L]][[2L]][[1L]]) != as.name("|") || formula[[3L]][[1L]] != as.name("|")){
            stop("Incorrect specification for 'formula'")
        }

        if (length(formula[[3L]][[2L]][[2L]]) != 3){
            wide.formula <- T
        }else if (formula[[3L]][[2L]][[2L]][[1L]] == as.name('+')) {
            wide.formula <- T
        }else if (formula[[3L]][[2L]][[2L]][[1L]] == as.name(':')) {
            long.formula <- T
        }else{
            stop("Incorrect specification for 'formula'")
        }
    }

    if(long.formula){
        # More error checking should go here to check for correct format ...
        return(.jocomo.chisq.test.formula.long(formula = formula, data = data, ...))
    } else if (xtabs.formula) {
        # More error checking should go here to check for correct format ...
        return(.jocomo.chisq.test.formula.xtabs(formula = formula, data = data, ...))
    } else if(wide.formula) {
        # Is it save to assume this is wide format?  I'm not sure, but for now I will ...
        return(.jocomo.chisq.test.formula.wide(formula = formula, data = data, ...))
    }else{
        stop("Error: unsupported formula")
    }
}

.jocomo.chisq.test.formula.long <- function(formula, data, ...){
    #stop("Not Implemented")
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
    samples <- mf[, 4L]
    groups <- mf[, 5L]

    ret <- jocomo::jocomo.chisq.test(x = x,
                                     y = y,
                                     models = models,
                                     samples = samples,
                                     groups = groups, ...)
    ret$data.name <- DNAME
    ret
}

.jocomo.chisq.test.formula.wide <- function(formula, data, ...){
    #stop("Not Implemented")
    formula[[3L]][[1L]] <- as.name("+")
    formula[[3L]][[2L]][[1L]] <- as.name("+")

    label.var <- formula[[2L]] |> as.character()
    sample.var <- formula[[3L]][[2L]][[3L]] |> as.character()
    group.var <- formula[[3L]][[3L]] |> as.character()

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

    y <- mf[[label.var]]
    #x <- mf[, -c(1L, dim(mf)[2L]-1L, dim(mf)[[2L]])]
    x <- subset(mf, select=setdiff(names(mf), c(label.var, sample.var, group.var)))
    samples <- mf[[sample.var]]
    groups <- mf[[group.var]]

    # samples <- mf[, c(dim(mf)[2L]-1L)]
    # groups <- mf[, c(dim(mf)[2L])]

    ret <- jocomo::jocomo.chisq.test(x = x, y = y,
                                     samples = samples, groups = groups,
                                     models = NULL, ...)
    ret$data.name <- DNAME
    ret
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
#' @inheritParams jocomo.chisq.test.default
#' @param xt An `xtabs` object of 3 or more dimensions indicating the
#' cross-tabulation of model predictions and true labels. Each factor must have
#' the same number of levels. The first dimension should refer to the true labels
#' while the remaining dimensions refer to the model predictions.
#' @param correct A boolean value indicating whether adjustments should be made
#' acount for empty cells in the contingency tables (required if any cells would be 0)
#' @return TODO
#' @rdname jocomo.chisq.test
#' @examples
#' 'TODO'
#' @exportS3Method jocomo::jocomo.chisq.test xtabs
jocomo.chisq.test.xtabs <- function(xt, correct=FALSE, ...) {
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


