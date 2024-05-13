#' An implementation of Wu's test from Wu 2023 (doi: 10.1080/10543406.2022.2065500)
#'
#' @param ... Additional arguments passed on to methods.
#' @rdname wu.test
#' @export
wu.test <- function(...) UseMethod("wu.test")

#' @param x Can be either a `matrix`, `data.frame`, or `vector` of model predictions.
#' As a `matrix` or `data.frame`, `x` should be a \eqn{p*q} `matrix` of binary
#' predictions with \eqn{p} subjects as rows and \eqn{q} models as columns. If
#' `x` is a `vector`, it should have length \eqn{p*q} and both `models` and
#' `subjects` must be specified. The data must be able to be coerced to a `factor`
#' with two levels. Ignored if a formula is specified.
#' @param y If `x` is a `matrix` or `data.frame`, then `y` must be a `vector` of
#' length \eqn{p} indicating positive and negative cases. If `x` is a `vector`,
#' then `y` must be a `vector` of length \eqn{p*q}. The data must be able to be
#' coerced to a `factor` with the same two levels as `x`. Ignored if a formula is
#' specified.
#' @param models A `vector` of length \eqn{p*q} indicating which model the datum
#' corresponds to. This should have \eqn{q} levels each occurring exactly \eqn{p}
#' times. Ignored if `x` is a `matrix` or `data.frame` or a formula is specified.
#' @param subjects A `vector` of length \eqn{p*q} indicating which subject the datum
#' corresponds to. This should have \eqn{p} levels each occurring exactly \eqn{q}
#' times. Ignored if `x` is a `matrix` or `data.frame` or a formula is specified.
#' @rdname wu.test
#' @method wu.test default
#' @exportS3Method jocomo::wu.test default
wu.test.default <- function(x, y, models, subjects, ...){
    # The following code is adapted from stats::friedman.test

    # If x is a matrix then we don't need to do much
    if (is.matrix(x) | is.data.frame(x)) {
        DNAME <- paste0(deparse(substitute(x)), " and ", deparse(substitute(y)))

        if (!is.matrix(x))
            x <- as.matrix(x)

        models <- factor(c(col(x)))
        subjects <- factor(c(row(x)))

        if(any(diff(c(length(y), dim(x)[1]))) != 0L){
            stop("Number of rows in x does not match length of y")
        }

        k <- nlevels(models)
    }
    else {
        # Make sure none of the subject/model entries are NA
        if (anyNA(subjects) || anyNA(models))
            stop("NA's are not allowed in 'models' or 'subjects'")
        # Make sure everything is the same length
        if (any(diff(c(length(y), length(models), length(subjects), length(x))) != 0L))
            stop("'x', 'y', 'models' and 'subjects' must have the same length")
        DNAME <- paste(deparse(substitute(x)), ', ', deparse(substitute(y)), ', ', deparse(substitute(models)),
                       ", and ", deparse(substitute(subjects)), sep = "")
        # Make sure we have complete data
        if (any(c(table(models, subjects)) == 0L))
            stop("There must be exactly one prediction from each model for each subject.  At least one is missing")
        if (any(c(table(models, subjects)) > 1L))
            stop("There must be exactly one prediction from each model for each subject.  At least one is duplicated")

        models <- factor(models)
        subjects <- factor(subjects)
        ## Need to ensure consistent order of observations within
        ## blocks.
        o <- order(models, subjects)
        y <- y[o]
        x <- x[o]
        models <- models[o]
        subjects <- subjects[o]
        k <- nlevels(models)
        y <- matrix(unlist(split(c(y), subjects)), ncol = k, byrow = TRUE)

        # Make sure y is consistent for each subject
        if (any(apply(y, 1, diff, simplify = T) != 0L))
            stop("'y' must not differ within levels of 'subjects'")
        # Remove unecessary data from y and convert to a vector ...
        y <- as.vector(y[,1])
    }

    ## <FIXME split.matrix> -- From friedman.test??
    x <- matrix(unlist(split(c(x), subjects)), ncol = k, byrow = TRUE)
    to.keep <- complete.cases(x) & complete.cases(y)
    y <- y[to.keep] # We only need the first column as it should not change
    x <- x[to.keep,]
    n <- nrow(x)

    # Convert everything to factors
    y <- factor(y, ordered=T)
    if (nlevels(y) != 2L)
        stop("'y' must have exactly 2 levels")
    x <- structure(factor(x, levels = levels(y), ordered=T), dim=dim(x), class=c('matrix', 'ordered', 'factor'))
    if (any(is.na(x)))
        stop("'x' must have the same levels as 'y'")

    STATISTIC <- wu.statistic(x = x, y = y)
    PARAMETER <- 2*(dim(x)[2]-1)
    PVAL <- stats::pchisq(STATISTIC, df = PARAMETER, lower.tail = FALSE)

    ## <FIXME split.matrix>
    names(STATISTIC) <- "Wu chi-squared"
    names(PARAMETER) <- "df"

    structure(list(statistic = STATISTIC,
                   parameter = PARAMETER,
                   p.value = PVAL,
                   method = "Wu's test",
                   data.name = DNAME),
              class = "htest")
}

#' @param formula
#' @rdname wu.test
#' @method wu.test formula
#' @exportS3Method jocomo::wu.test formula
wu.test.formula <- function(formula, data = NULL, ...){

    # Sanity checks
    if (missing(formula))
        stop("formula missing")
    if (length(formula) != 3L)
        stop("incorrect specification for 'formula'")

    wide.formula <- F
    if (typeof(formula[[3L]]) == as.name("language"))
        if (formula[[3L]][[1L]] != as.name('+'))
            wide.formula <- T
    # This formula follows the long format specification
    # if so, formula should be in the format of y~x:model|subject
    if (any(wide.formula)){
        if ((typeof(formula[[3L]][[1L]]) != as.name("symbol"))
            || (typeof(formula[[3L]][[2L]]) != as.name("language"))
            || (length(formula[[3L]][[2L]]) != 3L)
            || (typeof(formula[[3L]][[3L]]) != as.name("symbol")))
            stop("Incorrect specification for 'formula'")

        if ((formula[[3L]][[2L]][[1L]]) != as.name(":")
            ||formula[[3L]][[1L]] != as.name('|'))
            stop("Incorrect specification for 'formula'")

        return(.wu.test.formula.long(formula=formula, data=data, ...))
    }else{
      # Is it save to assume  this is wide format?
      # I'm not sure, but for now I will ...
        return(.wu.test.formula.wide(formula=formula, data=data, ...))
    }

}

.wu.test.formula.long <- function(formula, data = NULL, ...){

    formula[[3L]][[1L]] <- as.name('+')
    formula[[3L]][[2L]][[1L]] <- as.name('+')


    tf <- terms(formula, data=data)
    mf <- stats::model.frame(tf, data=data)

    if(dim(mf)[2L] != 4L)
        stop("Incorrect specification for 'formula'")

    nmf <- names(mf)
    DNAME <- paste0(paste0(names(mf)[1:(length(nmf)-1)], collapse = ", "),
                    ', and ', nmf[length(nmf)])

    y <- mf[,1L]
    x <- mf[,2L]
    models <- mf[,3L]
    subjects <- mf[,4L]

    ret <- wu.test(x = x, y = y, models = models, subjects = subjects, ...)
    ret$data.name <- DNAME
    ret

}

.wu.test.formula.wide <- function(formula, data = NULL, ...){
    #attr(tf, "intercept") <- 0
    if (!is.null(data)){
        tf <- terms(formula, data=data)
        mf <- stats::model.frame(tf, data = data)
    }else{
        tf <- terms(formula)
        mf <- stats::model.frame(tf)
    }

    nmf <- names(mf)
    if (length(nmf) == 2L){
        DNAME <- paste0(nmf, collapse = " and ")
    }else{
        DNAME <- paste0(paste0(nmf[1:(length(nmf)-1)], collapse = ", "),
                        ', and ', nmf[length(nmf)])
    }

    y <- mf[,1]
    x <- mf[,-1]

    ret <- wu.test(x = x, y = y, ...)
    ret$data.name <- DNAME
    ret

    # x <- model.matrix(tf, data=data)
    # y <- model.response(model.frame(tf, data=data))
    #wu.test.default(x=x, y=y, ...)
}


#' An implementation of the extended McNemar statistic from Wu 2023 (doi: 10.1080/10543406.2022.2065500)
#'
#' @inheritParams wu.test
#' @param correct Add 0.5 to each cell of the 2x2 contingency table to adjust for 0 counts
#' @export
wu.statistic <- function(x, y, correct=F){
    x <- structure(x==max(x), dim=dim(x), class=c('matrix', 'logical'))
    y <- y==max(y)

    p <- dim(x)[1]
    q <- dim(x)[2]

    x.pred.pos.cases <- x[y,]
    x.pred.neg.cases <- x[!y,]

    n.pos <- sum(y)
    n.neg <- sum(!y)

    k <- 1
    n01 <- c()
    n10 <- c()
    m01 <- c()
    m10 <- c()

    p1 <- x.pred.pos.cases[,1] |> factor(levels=c(F,T))
    n1 <- x.pred.neg.cases[,1] |> factor(levels=c(F,T))
    for(i in 2:q){
        p2 <- x.pred.pos.cases[,i] |> factor(levels=c(F,T))
        n2 <- x.pred.neg.cases[,i] |> factor(levels=c(F,T))
        for(j in 2:q){
            p3 <- x.pred.pos.cases[,j] |> factor(levels=c(F,T))
            n3 <- x.pred.neg.cases[,j] |> factor(levels=c(F,T))

            tb.pos <- table(p1, p2, p3) + ifelse(correct, 0.5, 0)
            tb.neg <- table(n1, n2, n3) + ifelse(correct, 0.5, 0)

            n01[k] <- tb.pos[1,2,2]
            n10[k] <- tb.pos[2,1,1]
            m01[k] <- tb.neg[1,2,2]
            m10[k] <- tb.neg[2,1,1]

            k <- k+1
      }
    }

    N01 <- matrix(n01, nrow=q-1)
    N10 <- matrix(n10, nrow=q-1)
    M01 <- matrix(m01, nrow=q-1)
    M10 <- matrix(m10, nrow=q-1)

    a <- diag(N10)-diag(N01)
    A <- N10+N01

    b <- diag(M10)-diag(M01)
    B <- M10+M01

    as.numeric(
      t(a) %*% solve(A) %*% a +
      t(b) %*% solve(B) %*% b
    )
}

coronary.disease.tabulated <- data.frame(
  Freq=c(215, 571, 9, 20, 31, 152, 1, 24,
         22, 47, 13, 33, 16, 160, 25, 126),
  expand.grid(T3=factor(c(1,0), levels=c(0,1)),
              T2=factor(c(1,0), levels=c(0,1)),
              T1=factor(c(1,0), levels=c(0,1)),
              D=factor(c(1,0), levels=c(0,1)))
)

coronary.disease <- do.call(rbind,apply(coronary.disease.tabulated, 1, \(r){
   data.frame(D=rep(r['D'], r['Freq']),
              T1=rep(r['T1'], r['Freq']),
              T2=rep(r['T2'], r['Freq']),
              T3=rep(r['T3'], r['Freq']))
}))
