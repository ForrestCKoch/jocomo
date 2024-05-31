# test_that("multiplication works", {
#   expect_equal(2 * 2, 4)
# })

get.test.data.1 <- function() {
  y <- 1881209709 |>
    intToBits() |>
    as.logical()
  x <- c(
    1512222934, 2068776800, 522499376, 975421075,
    376266222, 1728131073, 499928810, 511640686
  ) |>
    intToBits() |>
    as.logical() |>
    matrix(nrow = length(y))

  statistic <- 15.018873
  names(statistic) <- "Wu chi-squared"
  pvalue <- 0.37686577
  df <- 14
  names(df) <- "df"
  list(x = x, y = y, statistic = statistic, pvalue = pvalue, df = df, method = "Wu's test")
}

####################
# No Formula
####################
test_that("test wu.statistic: stable", {
  test.data <- get.test.data.1()
  suppressMessages(attach(test.data, warn.conflicts = FALSE))
  stat.out <- wu.statistic(x, y)
  names(stat.out) <- c("Wu chi-squared")
  expect_equal(stat.out, statistic)
})

test_that("test wu.test.default: data is logical, x is matrix", {
  test.data <- get.test.data.1()
  suppressMessages(attach(test.data, warn.conflicts = FALSE))

  wu.test.out <- wu.test(x, y)

  # expect_invisible(wu.test.default(x, y))
  expect_length(wu.test.out, 5)
  expect_equal(names(wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(wu.test.out$statistic, statistic)
  expect_equal(wu.test.out$parameter, df)
  expect_equal(wu.test.out$p.value, pvalue)
  expect_equal(wu.test.out$method, method)
  expect_equal(wu.test.out$data.name, "x and y")
})

test_that("test wu.test.default: data is logical, x is data.frame", {
  test.data <- get.test.data.1()
  suppressMessages(attach(test.data, warn.conflicts = FALSE))

  x <- data.frame(x)
  wu.test.out <- wu.test(x, y)

  # expect_invisible(wu.test.default(x, y))
  expect_length(wu.test.out, 5)
  expect_equal(names(wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(wu.test.out$statistic, statistic)
  expect_equal(wu.test.out$parameter, df)
  expect_equal(wu.test.out$p.value, pvalue)
  expect_equal(wu.test.out$method, method)
  expect_equal(wu.test.out$data.name, "x and y")
})

test_that("test wu.test.default: data is numeric, x is matrix", {
  test.data <- get.test.data.1()
  suppressMessages(attach(test.data, warn.conflicts = FALSE))
  x <- matrix(as.numeric(c(x)), nrow = length(y))
  y <- as.numeric(y)

  wu.test.out <- wu.test(x, y)

  # expect_invisible(wu.test.default(x, y))
  expect_length(wu.test.out, 5)
  expect_equal(names(wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(wu.test.out$statistic, statistic)
  expect_equal(wu.test.out$parameter, df)
  expect_equal(wu.test.out$p.value, pvalue)
  expect_equal(wu.test.out$method, method)
  expect_equal(wu.test.out$data.name, "x and y")
})

test_that("test wu.test.default: data is character, x is matrix", {
  test.data <- get.test.data.1()
  suppressMessages(attach(test.data, warn.conflicts = FALSE))
  x <- matrix(as.character(c(x)), nrow = length(y))
  y <- as.character(y)

  wu.test.out <- wu.test(x, y)

  # expect_invisible(wu.test.default(x, y))
  expect_length(wu.test.out, 5)
  expect_equal(names(wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(wu.test.out$statistic, statistic)
  expect_equal(wu.test.out$parameter, df)
  expect_equal(wu.test.out$p.value, pvalue)
  expect_equal(wu.test.out$method, method)
  expect_equal(wu.test.out$data.name, "x and y")
})

test_that("test wu.test.default: data is character, x is matrix -- 2", {
  test.data <- get.test.data.1()
  suppressMessages(attach(test.data, warn.conflicts = FALSE))
  x <- matrix(LETTERS[as.numeric(c(x)) + 1], nrow = length(y))
  y <- LETTERS[as.numeric(c(y)) + 1]

  wu.test.out <- wu.test(x, y)

  # expect_invisible(wu.test.default(x, y))
  expect_length(wu.test.out, 5)
  expect_equal(names(wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(wu.test.out$statistic, statistic)
  expect_equal(wu.test.out$parameter, df)
  expect_equal(wu.test.out$p.value, pvalue)
  expect_equal(wu.test.out$method, method)
  expect_equal(wu.test.out$data.name, "x and y")
})

test_that("test wu.test.default: data is unordered factor, x is matrix", {
  test.data <- get.test.data.1()
  suppressMessages(attach(test.data, warn.conflicts = FALSE))
  x <- matrix(factor(LETTERS[as.numeric(c(x)) + 1]), nrow = length(y))
  y <- factor(LETTERS[as.numeric(c(y)) + 1])

  wu.test.out <- wu.test(x, y)

  # expect_invisible(wu.test.default(x, y))
  expect_length(wu.test.out, 5)
  expect_equal(names(wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(wu.test.out$statistic, statistic)
  expect_equal(wu.test.out$parameter, df)
  expect_equal(wu.test.out$p.value, pvalue)
  expect_equal(wu.test.out$method, method)
  expect_equal(wu.test.out$data.name, "x and y")
})

test_that("test wu.test.default: data is ordered factor, x is matrix", {
  test.data <- get.test.data.1()
  suppressMessages(attach(test.data, warn.conflicts = FALSE))
  x <- matrix(factor(LETTERS[as.numeric(c(x)) + 1], ordered = T), nrow = length(y))
  y <- factor(LETTERS[as.numeric(c(y)) + 1], ordered = T)

  wu.test.out <- wu.test(x, y)

  # expect_invisible(wu.test.default(x, y))
  expect_length(wu.test.out, 5)
  expect_equal(names(wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(wu.test.out$statistic, statistic)
  expect_equal(wu.test.out$parameter, df)
  expect_equal(wu.test.out$p.value, pvalue)
  expect_equal(wu.test.out$method, method)
  expect_equal(wu.test.out$data.name, "x and y")
})

test_that("test wu.test.default: data is logical, x is vector, models and samples are factors", {
  test.data <- get.test.data.1()
  suppressMessages(attach(test.data, warn.conflicts = FALSE))
  models <- factor(col(x))
  samples <- factor(row(x))
  y <- rep(y, dim(x)[2])
  x <- c(x)


  wu.test.out <- wu.test(x, y, models = models, samples = samples)

  # expect_invisible(wu.test.default(x, y))
  expect_length(wu.test.out, 5)
  expect_equal(names(wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(wu.test.out$statistic, statistic)
  expect_equal(wu.test.out$parameter, df)
  expect_equal(wu.test.out$p.value, pvalue)
  expect_equal(wu.test.out$method, method)
  expect_equal(wu.test.out$data.name, "x, y, models, and samples")
})

test_that("test wu.test.default: data is logical, x is vector, models and samples are characters", {
  test.data <- get.test.data.1()
  suppressMessages(attach(test.data, warn.conflicts = FALSE))
  models <- factor(col(x)) |> as.character()
  samples <- factor(row(x)) |> as.character()
  y <- rep(y, dim(x)[2])
  x <- c(x)


  wu.test.out <- wu.test(x, y, models = models, samples = samples)

  # expect_invisible(wu.test.default(x, y))
  expect_length(wu.test.out, 5)
  expect_equal(names(wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(wu.test.out$statistic, statistic)
  expect_equal(wu.test.out$parameter, df)
  expect_equal(wu.test.out$p.value, pvalue)
  expect_equal(wu.test.out$method, method)
  expect_equal(wu.test.out$data.name, "x, y, models, and samples")
})

#########################
# Wide format formula
#########################
test_that("test .wu.test.formula.wide: data is logical list, wu.test.formula(y~x)", {
  test.data <- get.test.data.1()

  wu.test.out <- wu.test(y ~ x, data = list(x = test.data$x, y = test.data$y))

  # expect_invisible(wu.test.default(x, y))
  expect_length(wu.test.out, 5)
  expect_equal(names(wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(wu.test.out$statistic, statistic)
  expect_equal(wu.test.out$parameter, df)
  expect_equal(wu.test.out$p.value, pvalue)
  expect_equal(wu.test.out$method, method)
  expect_equal(wu.test.out$data.name, "y and x")
})

test_that("test .wu.test.formula.wide: data is attached logical list, wu.test.formula(y~x)", {
  test.data <- get.test.data.1()
  suppressMessages(attach(test.data, warn.conflicts = FALSE))

  wu.test.out <- wu.test(y ~ x)

  # expect_invisible(wu.test.default(x, y))
  expect_length(wu.test.out, 5)
  expect_equal(names(wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(wu.test.out$statistic, statistic)
  expect_equal(wu.test.out$parameter, df)
  expect_equal(wu.test.out$p.value, pvalue)
  expect_equal(wu.test.out$method, method)
  expect_equal(wu.test.out$data.name, "y and x")
})

test_that("test .wu.test.formula.wide: data is logical data.frame, wu.test.formula(y~x, data=test.df)", {
  test.data <- get.test.data.1()

  test.df <- data.frame(x = test.data$x, y = test.data$y)
  wu.test.out <- wu.test(y ~ x, data = test.df)

  # expect_invisible(wu.test.default(x, y))
  expect_length(wu.test.out, 5)
  expect_equal(names(wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(wu.test.out$statistic, statistic)
  expect_equal(wu.test.out$parameter, df)
  expect_equal(wu.test.out$p.value, pvalue)
  expect_equal(wu.test.out$method, method)
  expect_equal(wu.test.out$data.name, "y and x")
})

test_that("test .wu.test.formula.wide: data is logical data.frame, wu.test.formula(y~., data=test.df)", {
  test.data <- get.test.data.1()

  test.df <- data.frame(x = test.data$x, y = test.data$y)
  wu.test.out <- wu.test(y ~ ., data = test.df)

  # expect_invisible(wu.test.default(x, y))
  expect_length(wu.test.out, 5)
  expect_equal(names(wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(wu.test.out$statistic, statistic)
  expect_equal(wu.test.out$parameter, df)
  expect_equal(wu.test.out$p.value, pvalue)
  expect_equal(wu.test.out$method, method)
  expect_equal(wu.test.out$data.name, "y, x.1, x.2, x.3, x.4, x.5, x.6, x.7, and x.8")
})

test_that("test .wu.test.formula.wide: data is attached logical data.frame, wu.test.formula(y~x)", {
  test.data <- get.test.data.1()

  test.df <- data.frame(x = test.data$x, y = test.data$y)
  attach(test.df, warn.conflicts = FALSE)
  wu.test.out <- wu.test(y ~ x)

  # expect_invisible(wu.test.default(x, y))
  expect_length(wu.test.out, 5)
  expect_equal(names(wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(wu.test.out$statistic, statistic)
  expect_equal(wu.test.out$parameter, df)
  expect_equal(wu.test.out$p.value, pvalue)
  expect_equal(wu.test.out$method, method)
  expect_equal(wu.test.out$data.name, "y and x")
})


test_that("test .wu.test.formula.wide: data is logical data.frame, wu.test.formula(y~X1+X2+X3+X4+X5+X6+X7+X8, data=test.df)", {
  test.data <- get.test.data.1()

  test.df <- data.frame(x = test.data$x, y = test.data$y)
  wu.test.out <- wu.test(y ~ x.1 + x.2 + x.3 + x.4 + x.5 + x.6 + x.7 + x.8, data = test.df)

  # expect_invisible(wu.test.default(x, y))
  expect_length(wu.test.out, 5)
  expect_equal(names(wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(wu.test.out$statistic, statistic)
  expect_equal(wu.test.out$parameter, df)
  expect_equal(wu.test.out$p.value, pvalue)
  expect_equal(wu.test.out$method, method)
  expect_equal(wu.test.out$data.name, "y, x.1, x.2, x.3, x.4, x.5, x.6, x.7, and x.8")
})

test_that("test .wu.test.formula.wide: data is attached logical data.frame, wu.test.formula(y~x.1+x.2+x.3+x.4+x.5+x.6+x.7+x.8, data=test.df)", {
  test.data <- get.test.data.1()

  test.df <- data.frame(x = test.data$x, y = test.data$y)
  attach(test.df, warn.conflicts = FALSE)
  wu.test.out <- wu.test(y ~ x.1 + x.2 + x.3 + x.4 + x.5 + x.6 + x.7 + x.8)

  # expect_invisible(wu.test.default(x, y))
  expect_length(wu.test.out, 5)
  expect_equal(names(wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(wu.test.out$statistic, statistic)
  expect_equal(wu.test.out$parameter, df)
  expect_equal(wu.test.out$p.value, pvalue)
  expect_equal(wu.test.out$method, method)
  expect_equal(wu.test.out$data.name, "y, x.1, x.2, x.3, x.4, x.5, x.6, x.7, and x.8")
})

#########################
# Long format formula
#########################

test_that("test .wu.test.formula.long: data.frame, y/x are logical, models and samples are factors", {
  test.data <- get.test.data.1()
  # suppressMessages(attach(test.data, warn.conflicts = FALSE))

  test.df <- data.frame(
    x = c(test.data$x),
    y = rep(test.data$y, dim(test.data$x)[2]),
    models = factor(col(test.data$x)),
    samples = factor(row(test.data$x))
  )


  wu.test.out <- wu.test(y ~ x:models | samples, data = test.df)

  # expect_invisible(wu.test.default(x, y))
  expect_length(wu.test.out, 5)
  expect_equal(names(wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(wu.test.out$statistic, statistic)
  expect_equal(wu.test.out$parameter, df)
  expect_equal(wu.test.out$p.value, pvalue)
  expect_equal(wu.test.out$method, method)
  expect_equal(wu.test.out$data.name, "y, x, models, and samples")
})

###########################
# Cross tabulated
###########################

test_that("test wu.test.xtabs: data is cross tab", {
  test.data <- get.test.data.1()

  test.xt <- xtabs(~ y + ., data = data.frame(x = test.data$x, y = test.data$y))
  wu.test.out <- wu.test(test.xt)

  # expect_invisible(wu.test.default(x, y))
  expect_length(wu.test.out, 5)
  expect_equal(names(wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(wu.test.out$statistic, statistic)
  expect_equal(wu.test.out$parameter, df)
  expect_equal(wu.test.out$p.value, pvalue)
  expect_equal(wu.test.out$method, method)
  # expect_equal(wu.test.out$data.name, 'y and x')
})

test_that("test .wu.test.formula.xtabs: data is cross tab, wu.test.formula(Freq~.||y)", {
  test.data <- get.test.data.1()

  test.df <- data.frame(xtabs(~ y + ., data = data.frame(x = test.data$x, y = test.data$y)))
  wu.test.out <- wu.test(Freq ~ . || y, data = test.df)

  # expect_invisible(wu.test.default(x, y))
  expect_length(wu.test.out, 5)
  expect_equal(names(wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(wu.test.out$statistic, statistic)
  expect_equal(wu.test.out$parameter, df)
  expect_equal(wu.test.out$p.value, pvalue)
  expect_equal(wu.test.out$method, method)
  # expect_equal(wu.test.out$data.name, 'y and x')
})

#####################################
# Testing coronary.disease.tabulated
#####################################

test_that("test coronary.disease.tabulated: xtabs", {
  wu.test.out <- wu.test(xtabs(Freq ~ D + .,  data = coronary.disease.tabulated))

  statistic <- 811.3
  names(statistic) <- "Wu chi-squared"
  pvalue <- 0
  df <- 4
  names(df) <- "df"

  # expect_invisible(wu.test.default(x, y))
  expect_length(wu.test.out, 5)
  expect_equal(names(wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(wu.test.out$statistic, statistic, tolerance = 1e-2)
  expect_equal(wu.test.out$parameter, df)
  expect_equal(wu.test.out$p.value, pvalue)
  expect_equal(wu.test.out$method, "Wu's test")
  # expect_equal(wu.test.out$data.name, 'y and x')
})

test_that("test coronary.disease.tabulated: xtabs formula", {
  wu.test.out <- wu.test(Freq ~ . || D,  data = coronary.disease.tabulated)

  statistic <- 811.3
  names(statistic) <- "Wu chi-squared"
  pvalue <- 0
  df <- 4
  names(df) <- "df"

  # expect_invisible(wu.test.default(x, y))
  expect_length(wu.test.out, 5)
  expect_equal(names(wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(wu.test.out$statistic, statistic, tolerance = 1e-2)
  expect_equal(wu.test.out$parameter, df)
  expect_equal(wu.test.out$p.value, pvalue)
  expect_equal(wu.test.out$method, "Wu's test")
  # expect_equal(wu.test.out$data.name, 'y and x')
})

test_that("test coronary.disease.tabulated: xtabs formula", {
  wu.test.out <- wu.test(Freq ~ T1 + T2 + T3 || D,  data = coronary.disease.tabulated)

  statistic <- 811.3
  names(statistic) <- "Wu chi-squared"
  pvalue <- 0
  df <- 4
  names(df) <- "df"

  # expect_invisible(wu.test.default(x, y))
  expect_length(wu.test.out, 5)
  expect_equal(names(wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(wu.test.out$statistic, statistic, tolerance = 1e-2)
  expect_equal(wu.test.out$parameter, df)
  expect_equal(wu.test.out$p.value, pvalue)
  expect_equal(wu.test.out$method, "Wu's test")
  # expect_equal(wu.test.out$data.name, 'y and x')
})

test_that("test coronary.disease.wide: formula", {
  wu.test.out <- wu.test(D ~ .,  data = coronary.disease.wide)

  statistic <- 811.3
  names(statistic) <- "Wu chi-squared"
  pvalue <- 0
  df <- 4
  names(df) <- "df"

  # expect_invisible(wu.test.default(x, y))
  expect_length(wu.test.out, 5)
  expect_equal(names(wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(wu.test.out$statistic, statistic, tolerance = 1e-2)
  expect_equal(wu.test.out$parameter, df)
  expect_equal(wu.test.out$p.value, pvalue)
  expect_equal(wu.test.out$method, "Wu's test")
  # expect_equal(wu.test.out$data.name, 'y and x')
})

test_that("test coronary.disease.long: default", {
  wu.test.out <- wu.test(x=coronary.disease.long$x,
                         y=coronary.disease.long$D,
                         samples=coronary.disease.long$sample,
                         models=coronary.disease.long$model)

  statistic <- 811.3
  names(statistic) <- "Wu chi-squared"
  pvalue <- 0
  df <- 4
  names(df) <- "df"

  # expect_invisible(wu.test.default(x, y))
  expect_length(wu.test.out, 5)
  expect_equal(names(wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(wu.test.out$statistic, statistic, tolerance = 1e-2)
  expect_equal(wu.test.out$parameter, df)
  expect_equal(wu.test.out$p.value, pvalue)
  expect_equal(wu.test.out$method, "Wu's test")
  # expect_equal(wu.test.out$data.name, 'y and x')
})

test_that("test coronary.disease.long: formula", {
  wu.test.out <- wu.test(D ~ x:model|sample,  data = coronary.disease.long)

  statistic <- 811.3
  names(statistic) <- "Wu chi-squared"
  pvalue <- 0
  df <- 4
  names(df) <- "df"

  # expect_invisible(wu.test.default(x, y))
  expect_length(wu.test.out, 5)
  expect_equal(names(wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(wu.test.out$statistic, statistic, tolerance = 1e-2)
  expect_equal(wu.test.out$parameter, df)
  expect_equal(wu.test.out$p.value, pvalue)
  expect_equal(wu.test.out$method, "Wu's test")
  # expect_equal(wu.test.out$data.name, 'y and x')
})
