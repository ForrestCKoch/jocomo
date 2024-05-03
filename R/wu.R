#' @rdname wu.test
#' @method wu.test default
#' @exportS3Method jocomo::wu.test default
wu.test.default <- function(x, y, models, subjects, ...){
  # J <- dim(x)[2]
  # X_EM <- wu.statistic(x=x, y=y)
  # p.value <- stats::pchisq(X_EM, df=2*(J-1), lower.tail=F)
  # list(statistic=X_EM, p.value=p.value)

  # The following code is essentially copied and reworked from stats::friedman.test

  DNAME <- paste0(deparse(substitute(x)), " and ", deparse(substitute(y)))

  # If x is a matrix then we don't need to do much
  if (is.matrix(x)) {
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
    DNAME <- paste(DNAME, ", ", deparse(substitute(models)),
                   " and ", deparse(substitute(subjects)), sep = "")
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

#' @rdname wu.test
#' @method wu.test formula
#' @exportS3Method jocomo::wu.test formula
wu.test.formula <- function(formula, data, wide=T,...){
  if(wide){
    return(wu.test.formula.wide(formula=formula, data=data, ...))
  }else{
    return(wu.test.formula.long(formula=formula, data=data, ...))
  }
}

wu.test.formula.wide <- function(formula, data, ...){
  tf <- terms(formula, data)
  attr(tf, "intercept") <- 0
  x <- model.matrix(tf, data=data)
  y <- model.response(model.frame(tf, data=data))
  wu.test.default(x=x, y=y, ...)
}

#' An implementation of Wu's test from Wu 2023 (doi: 10.1080/10543406.2022.2065500)
#'
#' @param x a \eqn{p*q} matrix of binary predictions with \eqn{p} subjects as rows and \eqn{q} models as columns.
#' @param y a logical vector of length \eqn{p} indicating the positive cases.
#' @param ... further arguments to be passed to or from methods.
#' @rdname wu.test
#' @export
wu.test <- function(x, ...){
  UseMethod("wu.test")
}

#' An implementation of the extended McNemar statistic from Wu 2023 (doi: 10.1080/10543406.2022.2065500)
#'
#' @inheritParams wu.test
#' @param correct Add 0.5 to each cell of the 2x2 contingency table to adjust for 0 counts
#' @export
wu.statistic <- function(x, y, correct=F){
  x.pred <- x

  p <- dim(x)[1]
  q <- dim(x)[2]

  x.pred.pos.cases <- x.pred[y,]
  x.pred.neg.cases <- x.pred[!y,]

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

friedman.test.default <-
  function(y, groups, blocks, ...)
  {
    DNAME <- deparse(substitute(y))
    if (is.matrix(y)) {
      groups <- factor(c(col(y)))
      blocks <- factor(c(row(y)))
    }
    else {
      if (anyNA(groups) || anyNA(blocks))
        stop("NA's are not allowed in 'groups' or 'blocks'")
      if (any(diff(c(length(y), length(groups), length(blocks))) != 0L))
        stop("'y', 'groups' and 'blocks' must have the same length")
      DNAME <- paste(DNAME, ", ", deparse(substitute(groups)),
                     " and ", deparse(substitute(blocks)), sep = "")
      if (any(table(groups, blocks) != 1))
        stop("not an unreplicated complete block design")
      groups <- factor(groups)
      blocks <- factor(blocks)
      ## Need to ensure consistent order of observations within
      ## blocks.
      o <- order(groups, blocks)
      y <- y[o]
      groups <- groups[o]
      blocks <- blocks[o]
    }

    k <- nlevels(groups)
    ## <FIXME split.matrix>
    y <- matrix(unlist(split(c(y), blocks)), ncol = k, byrow = TRUE)
    y <- y[complete.cases(y), ]
    n <- nrow(y)
    r <- t(apply(y, 1L, rank))
    ## <FIXME split.matrix>
    TIES <- tapply(c(r), row(r), table)
    STATISTIC <- ((12 * sum((colSums(r) - n * (k + 1) / 2)^2)) /
                    (n * k * (k + 1)
                     - (sum(unlist(lapply(TIES, function (u) {u^3 - u}))) /
                          (k - 1))))
    PARAMETER <- k - 1
    PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
    names(STATISTIC) <- "Friedman chi-squared"
    names(PARAMETER) <- "df"

    structure(list(statistic = STATISTIC,
                   parameter = PARAMETER,
                   p.value = PVAL,
                   method = "Friedman rank sum test",
                   data.name = DNAME),
              class = "htest")
  }

friedman.test.formula <-
  function(formula, data, subset, na.action, ...)
  {
    if(missing(formula))
      stop("formula missing")
    ## <FIXME>
    ## Maybe put this into an internal rewriteTwoWayFormula() when
    ## adding support for strata()
    if((length(formula) != 3L)
       || (length(formula[[3L]]) != 3L)
       || (formula[[3L]][[1L]] != as.name("|"))
       || (length(formula[[3L]][[2L]]) != 1L)
       || (length(formula[[3L]][[3L]]) != 1L))
      stop("incorrect specification for 'formula'")
    formula[[3L]][[1L]] <- as.name("+")
    ## </FIXME>
    m <- match.call(expand.dots = FALSE)
    m$formula <- formula
    if(is.matrix(eval(m$data, parent.frame())))
      m$data <- as.data.frame(data)
    ## need stats:: for non-standard evaluation
    m[[1L]] <- quote(stats::model.frame)
    mf <- eval(m, parent.frame())
    DNAME <- paste(names(mf), collapse = " and ")
    names(mf) <- NULL
    y <- do.call("friedman.test", as.list(mf))
    y$data.name <- DNAME
    y
  }
