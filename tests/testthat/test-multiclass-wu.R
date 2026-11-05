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
  names(statistic) <- "chi-squared"
  pvalue <- 0.37686577
  df <- 14
  names(df) <- "df"
  list(x = x, y = y, statistic = statistic, pvalue = pvalue, df = df, method = "Multiclass Wu's test")
}

test_that("multiclass Wu supports two models", {
  x <- matrix(
    c("a", "a", "b", "b",
      "b", "a", "a", "b"),
    nrow = 4L,
    byrow = FALSE
  )
  y <- factor(c("a", "a", "b", "b"))

  statistic <- multiclass.wu.statistic(x, y)

  expect_type(statistic, "double")
  expect_length(statistic, 1L)
  expect_true(is.finite(statistic))
})

test_that("multiclass Wu handles singular tables explicitly", {
  x <- matrix("a", nrow = 4L, ncol = 2L)
  y <- factor(c("a", "a", "b", "b"))

  expect_error(
    multiclass.wu.statistic(x, y),
    "covariance matrix is singular"
  )
  corrected <- multiclass.wu.statistic(x, y, correct = TRUE)
  expect_true(is.finite(corrected))
  expect_identical(
    corrected,
    multiclass.wu.statistic(x, y, correct = TRUE)
  )
})

test_that("multiclass Wu remains finite for weighted and near-singular tables", {
  x <- matrix(c(
    "a", "a", "a", "b", "b", "b",
    "a", "a", "b", "a", "b", "b",
    "a", "b", "a", "b", "a", "b"
  ), nrow = 6L)
  y <- factor(c("a", "a", "a", "b", "b", "b"))
  weights <- c(1e12, 1, 1, 1, 1e12, 1)

  expect_true(is.finite(
    jocomo:::.multiclass_wu_statistic_from_data(
      x, y, weights = weights, correct = TRUE
    )
  ))
})

test_that("multiclass Wu validates inputs and missing predictions", {
  x <- matrix(c("a", "a", "b", "b", "a", "a", "b", "b"),
              nrow = 4L)
  y <- factor(c("a", "a", "b", "b"))

  expect_error(
    multiclass.wu.statistic(x[, 1L, drop = FALSE], y),
    "at least 2 models"
  )
  expect_error(
    multiclass.wu.statistic(x, y[-1L]),
    "one value for each row"
  )
  expect_error(
    multiclass.wu.statistic(matrix(NA_character_, nrow = 4L, ncol = 2L), y),
    "must not contain missing values"
  )
  expect_s3_class(
    multiclass.wu.test(
      matrix(c("a", "b", "a", "a", "c", "a", "c", "b"),
             nrow = 4L, byrow = TRUE),
      y
    ),
    "htest"
  )
  expect_error(
    multiclass.wu.statistic(x, y, correct = NA),
    "single non-missing logical value"
  )
})

test_that("multiclass Wu validates tabulated frequencies", {
  data <- data.frame(
    Freq = c(1, 2, 3, 4),
    y = factor(c("a", "a", "b", "b")),
    x1 = factor(c("a", "b", "a", "b")),
    x2 = factor(c("a", "b", "b", "a"))
  )
  xt <- xtabs(Freq ~ y + x1 + x2, data = data)
  expect_error(
    multiclass.wu.statistic(
      xtabs(Freq ~ y + x1 + x2,
            data = transform(data, Freq = -Freq))
    ),
    "finite, numeric, and non-negative"
  )
  expect_error(
    multiclass.wu.statistic(xtabs(Freq ~ y + x1, data = data)),
    "at least 2 models"
  )
})

test_that("multiclass Wu supports three-class xtabs inputs", {
  data <- data.frame(
    y = factor(c("a", "a", "a", "b", "b", "b", "c", "c", "c")),
    x1 = factor(c("a", "b", "c", "b", "c", "a", "c", "a", "b")),
    x2 = factor(c("a", "c", "b", "c", "a", "b", "b", "c", "a")),
    x3 = factor(c("b", "a", "c", "a", "c", "b", "c", "b", "a"))
  )
  table <- xtabs(~ y + x1 + x2 + x3, data = data)

  raw <- multiclass.wu.statistic(
    as.matrix(data[c("x1", "x2", "x3")]),
    data$y,
    correct = TRUE
  )
  tabulated <- multiclass.wu.statistic(table, correct = TRUE)

  expect_true(is.finite(raw))
  expect_equal(tabulated, raw)
  expect_equal(
    unname(multiclass.wu.test(table, correct = TRUE)$parameter),
    3L * 2L
  )
})

####################
# No Formula
####################
test_that("test wu.statistic: stable", {
  test.data <- get.test.data.1()
  suppressMessages(attach(test.data, warn.conflicts = FALSE))
  stat.out <- wu.statistic(x, y)
  names(stat.out) <- c("chi-squared")
  expect_equal(stat.out, statistic)
})

test_that("test multiclass.wu.test.default: data is logical, x is matrix", {
  test.data <- get.test.data.1()
  suppressMessages(attach(test.data, warn.conflicts = FALSE))

  multiclass.wu.test.out <- multiclass.wu.test(x, y)

  # expect_invisible(multiclass.wu.test.default(x, y))
  expect_length(multiclass.wu.test.out, 5)
  expect_equal(names(multiclass.wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(multiclass.wu.test.out$statistic, statistic)
  expect_equal(multiclass.wu.test.out$parameter, df)
  expect_equal(multiclass.wu.test.out$p.value, pvalue)
  expect_equal(multiclass.wu.test.out$method, method)
  expect_equal(multiclass.wu.test.out$data.name, "x and y")
})

test_that("test multiclass.wu.test.default: data is logical, x is data.frame", {
  test.data <- get.test.data.1()
  suppressMessages(attach(test.data, warn.conflicts = FALSE))

  x <- data.frame(x)
  multiclass.wu.test.out <- multiclass.wu.test(x, y)

  # expect_invisible(multiclass.wu.test.default(x, y))
  expect_length(multiclass.wu.test.out, 5)
  expect_equal(names(multiclass.wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(multiclass.wu.test.out$statistic, statistic)
  expect_equal(multiclass.wu.test.out$parameter, df)
  expect_equal(multiclass.wu.test.out$p.value, pvalue)
  expect_equal(multiclass.wu.test.out$method, method)
  expect_equal(multiclass.wu.test.out$data.name, "x and y")
})

test_that("test multiclass.wu.test.default: data is numeric, x is matrix", {
  test.data <- get.test.data.1()
  suppressMessages(attach(test.data, warn.conflicts = FALSE))
  x <- matrix(as.numeric(c(x)), nrow = length(y))
  y <- as.numeric(y)

  multiclass.wu.test.out <- multiclass.wu.test(x, y)

  # expect_invisible(multiclass.wu.test.default(x, y))
  expect_length(multiclass.wu.test.out, 5)
  expect_equal(names(multiclass.wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(multiclass.wu.test.out$statistic, statistic)
  expect_equal(multiclass.wu.test.out$parameter, df)
  expect_equal(multiclass.wu.test.out$p.value, pvalue)
  expect_equal(multiclass.wu.test.out$method, method)
  expect_equal(multiclass.wu.test.out$data.name, "x and y")
})

test_that("test multiclass.wu.test.default: data is character, x is matrix", {
  test.data <- get.test.data.1()
  suppressMessages(attach(test.data, warn.conflicts = FALSE))
  x <- matrix(as.character(c(x)), nrow = length(y))
  y <- as.character(y)

  multiclass.wu.test.out <- multiclass.wu.test(x, y)

  # expect_invisible(multiclass.wu.test.default(x, y))
  expect_length(multiclass.wu.test.out, 5)
  expect_equal(names(multiclass.wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(multiclass.wu.test.out$statistic, statistic)
  expect_equal(multiclass.wu.test.out$parameter, df)
  expect_equal(multiclass.wu.test.out$p.value, pvalue)
  expect_equal(multiclass.wu.test.out$method, method)
  expect_equal(multiclass.wu.test.out$data.name, "x and y")
})

test_that("test multiclass.wu.test.default: data is character, x is matrix -- 2", {
  test.data <- get.test.data.1()
  suppressMessages(attach(test.data, warn.conflicts = FALSE))
  x <- matrix(LETTERS[as.numeric(c(x)) + 1], nrow = length(y))
  y <- LETTERS[as.numeric(c(y)) + 1]

  multiclass.wu.test.out <- multiclass.wu.test(x, y)

  # expect_invisible(multiclass.wu.test.default(x, y))
  expect_length(multiclass.wu.test.out, 5)
  expect_equal(names(multiclass.wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(multiclass.wu.test.out$statistic, statistic)
  expect_equal(multiclass.wu.test.out$parameter, df)
  expect_equal(multiclass.wu.test.out$p.value, pvalue)
  expect_equal(multiclass.wu.test.out$method, method)
  expect_equal(multiclass.wu.test.out$data.name, "x and y")
})

test_that("test multiclass.wu.test.default: data is unordered factor, x is matrix", {
  test.data <- get.test.data.1()
  suppressMessages(attach(test.data, warn.conflicts = FALSE))
  x <- matrix(factor(LETTERS[as.numeric(c(x)) + 1]), nrow = length(y))
  y <- factor(LETTERS[as.numeric(c(y)) + 1])

  multiclass.wu.test.out <- multiclass.wu.test(x, y)

  # expect_invisible(multiclass.wu.test.default(x, y))
  expect_length(multiclass.wu.test.out, 5)
  expect_equal(names(multiclass.wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(multiclass.wu.test.out$statistic, statistic)
  expect_equal(multiclass.wu.test.out$parameter, df)
  expect_equal(multiclass.wu.test.out$p.value, pvalue)
  expect_equal(multiclass.wu.test.out$method, method)
  expect_equal(multiclass.wu.test.out$data.name, "x and y")
})

test_that("test multiclass.wu.test.default: data is ordered factor, x is matrix", {
  test.data <- get.test.data.1()
  suppressMessages(attach(test.data, warn.conflicts = FALSE))
  x <- matrix(factor(LETTERS[as.numeric(c(x)) + 1], ordered = T), nrow = length(y))
  y <- factor(LETTERS[as.numeric(c(y)) + 1], ordered = T)

  multiclass.wu.test.out <- multiclass.wu.test(x, y)

  # expect_invisible(multiclass.wu.test.default(x, y))
  expect_length(multiclass.wu.test.out, 5)
  expect_equal(names(multiclass.wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(multiclass.wu.test.out$statistic, statistic)
  expect_equal(multiclass.wu.test.out$parameter, df)
  expect_equal(multiclass.wu.test.out$p.value, pvalue)
  expect_equal(multiclass.wu.test.out$method, method)
  expect_equal(multiclass.wu.test.out$data.name, "x and y")
})

test_that("test multiclass.wu.test.default: data is logical, x is vector, models and samples are factors", {
  test.data <- get.test.data.1()
  suppressMessages(attach(test.data, warn.conflicts = FALSE))
  models <- factor(col(x))
  samples <- factor(row(x))
  y <- rep(y, dim(x)[2])
  x <- c(x)


  multiclass.wu.test.out <- multiclass.wu.test(x, y, models = models, samples = samples)

  # expect_invisible(multiclass.wu.test.default(x, y))
  expect_length(multiclass.wu.test.out, 5)
  expect_equal(names(multiclass.wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(multiclass.wu.test.out$statistic, statistic)
  expect_equal(multiclass.wu.test.out$parameter, df)
  expect_equal(multiclass.wu.test.out$p.value, pvalue)
  expect_equal(multiclass.wu.test.out$method, method)
  expect_equal(multiclass.wu.test.out$data.name, "x, y, models, and samples")
})

test_that("test multiclass.wu.test.default: data is logical, x is vector, models and samples are characters", {
  test.data <- get.test.data.1()
  suppressMessages(attach(test.data, warn.conflicts = FALSE))
  models <- factor(col(x)) |> as.character()
  samples <- factor(row(x)) |> as.character()
  y <- rep(y, dim(x)[2])
  x <- c(x)


  multiclass.wu.test.out <- multiclass.wu.test(x, y, models = models, samples = samples)

  # expect_invisible(multiclass.wu.test.default(x, y))
  expect_length(multiclass.wu.test.out, 5)
  expect_equal(names(multiclass.wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(multiclass.wu.test.out$statistic, statistic)
  expect_equal(multiclass.wu.test.out$parameter, df)
  expect_equal(multiclass.wu.test.out$p.value, pvalue)
  expect_equal(multiclass.wu.test.out$method, method)
  expect_equal(multiclass.wu.test.out$data.name, "x, y, models, and samples")
})

#########################
# Wide format formula
#########################
test_that("multiclass formula handling is strict and non-mutating", {
  test.data <- get.test.data.1()
  test.df <- data.frame(
    y = test.data$y,
    x1 = test.data$x[, 1L],
    x2 = test.data$x[, 2L]
  )

  long_formula <- y ~ x:model | sample
  long_formula_copy <- long_formula
  long_data <- data.frame(
    y = rep(test.df$y, 2L),
    x = c(test.df$x1, test.df$x2),
    model = factor(rep(c("m1", "m2"), each = nrow(test.df))),
    sample = factor(rep(seq_len(nrow(test.df)), 2L))
  )
  multiclass.wu.test(long_formula, data = long_data)
  expect_identical(long_formula, long_formula_copy)

  expect_error(
    multiclass.wu.test(y ~ x | sample, data = long_data),
    "Long formulas"
  )
  expect_error(
    multiclass.wu.test(y ~ x1 / x2, data = test.df),
    "Unsupported formula RHS"
  )
})

test_that("multiclass formulas preserve local environments and validate data", {
  test.data <- get.test.data.1()
  local_formula_test <- function() {
    x1 <- test.data$x[, 1L]
    x2 <- test.data$x[, 2L]
    y <- test.data$y
    multiclass.wu.test(y ~ x1 + x2)
  }

  expect_s3_class(local_formula_test(), "htest")
  expect_error(
    multiclass.wu.test(y ~ x1 + x2, data = data.frame(
      y = test.data$y,
      x1 = test.data$x[, 1L],
      x2 = c(test.data$x[-1L, 2L], NA)
    )),
    "must not contain missing values"
  )
})

test_that("multiclass cross-tabulated formulas validate frequencies and models", {
  data <- data.frame(
    Freq = c(1, 2, 3, 4),
    y = factor(c("a", "a", "b", "b")),
    x1 = factor(c("a", "b", "a", "b")),
    x2 = factor(c("a", "b", "b", "a"))
  )

  expect_error(
    multiclass.wu.test(Freq ~ x1 || y, data = data),
    "at least 2 model predictions"
  )
  expect_error(
    multiclass.wu.test(Freq ~ x1 + x2 || y,
                       data = transform(data, Freq = -Freq)),
    "finite, numeric, and non-negative"
  )
  expect_error(
    multiclass.wu.test(Freq ~ x1 + x2 || y,
                       data = transform(data, Freq = NA_real_)),
    "must not contain missing values"
  )
})

test_that("test .multiclass.wu.test.formula.wide: data is logical list, multiclass.wu.test.formula(y~x)", {
  test.data <- get.test.data.1()

  multiclass.wu.test.out <- multiclass.wu.test(y ~ x, data = list(x = test.data$x, y = test.data$y))

  # expect_invisible(multiclass.wu.test.default(x, y))
  expect_length(multiclass.wu.test.out, 5)
  expect_equal(names(multiclass.wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(multiclass.wu.test.out$statistic, statistic)
  expect_equal(multiclass.wu.test.out$parameter, df)
  expect_equal(multiclass.wu.test.out$p.value, pvalue)
  expect_equal(multiclass.wu.test.out$method, method)
  expect_equal(multiclass.wu.test.out$data.name, "y and x")
})

test_that("test .multiclass.wu.test.formula.wide: data is attached logical list, multiclass.wu.test.formula(y~x)", {
  test.data <- get.test.data.1()
  suppressMessages(attach(test.data, warn.conflicts = FALSE))

  multiclass.wu.test.out <- multiclass.wu.test(y ~ x)

  # expect_invisible(multiclass.wu.test.default(x, y))
  expect_length(multiclass.wu.test.out, 5)
  expect_equal(names(multiclass.wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(multiclass.wu.test.out$statistic, statistic)
  expect_equal(multiclass.wu.test.out$parameter, df)
  expect_equal(multiclass.wu.test.out$p.value, pvalue)
  expect_equal(multiclass.wu.test.out$method, method)
  expect_equal(multiclass.wu.test.out$data.name, "y and x")
})

test_that("test .multiclass.wu.test.formula.wide: data is logical data.frame, multiclass.wu.test.formula(y~x, data=test.df)", {
  test.data <- get.test.data.1()

  test.df <- data.frame(x = test.data$x, y = test.data$y)
  multiclass.wu.test.out <- multiclass.wu.test(y ~ x, data = test.df)

  # expect_invisible(multiclass.wu.test.default(x, y))
  expect_length(multiclass.wu.test.out, 5)
  expect_equal(names(multiclass.wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(multiclass.wu.test.out$statistic, statistic)
  expect_equal(multiclass.wu.test.out$parameter, df)
  expect_equal(multiclass.wu.test.out$p.value, pvalue)
  expect_equal(multiclass.wu.test.out$method, method)
  expect_equal(multiclass.wu.test.out$data.name, "y and x")
})

test_that("test .multiclass.wu.test.formula.wide: data is logical data.frame, multiclass.wu.test.formula(y~., data=test.df)", {
  test.data <- get.test.data.1()

  test.df <- data.frame(x = test.data$x, y = test.data$y)
  multiclass.wu.test.out <- multiclass.wu.test(y ~ ., data = test.df)

  # expect_invisible(multiclass.wu.test.default(x, y))
  expect_length(multiclass.wu.test.out, 5)
  expect_equal(names(multiclass.wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(multiclass.wu.test.out$statistic, statistic)
  expect_equal(multiclass.wu.test.out$parameter, df)
  expect_equal(multiclass.wu.test.out$p.value, pvalue)
  expect_equal(multiclass.wu.test.out$method, method)
  expect_equal(multiclass.wu.test.out$data.name, "y, x.1, x.2, x.3, x.4, x.5, x.6, x.7, and x.8")
})

test_that("test .multiclass.wu.test.formula.wide: data is attached logical data.frame, multiclass.wu.test.formula(y~x)", {
  test.data <- get.test.data.1()

  test.df <- data.frame(x = test.data$x, y = test.data$y)
  attach(test.df, warn.conflicts = FALSE)
  multiclass.wu.test.out <- multiclass.wu.test(y ~ x)

  # expect_invisible(multiclass.wu.test.default(x, y))
  expect_length(multiclass.wu.test.out, 5)
  expect_equal(names(multiclass.wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(multiclass.wu.test.out$statistic, statistic)
  expect_equal(multiclass.wu.test.out$parameter, df)
  expect_equal(multiclass.wu.test.out$p.value, pvalue)
  expect_equal(multiclass.wu.test.out$method, method)
  expect_equal(multiclass.wu.test.out$data.name, "y and x")
})


test_that("test .multiclass.wu.test.formula.wide: data is logical data.frame, multiclass.wu.test.formula(y~X1+X2+X3+X4+X5+X6+X7+X8, data=test.df)", {
  test.data <- get.test.data.1()

  test.df <- data.frame(x = test.data$x, y = test.data$y)
  multiclass.wu.test.out <- multiclass.wu.test(y ~ x.1 + x.2 + x.3 + x.4 + x.5 + x.6 + x.7 + x.8, data = test.df)

  # expect_invisible(multiclass.wu.test.default(x, y))
  expect_length(multiclass.wu.test.out, 5)
  expect_equal(names(multiclass.wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(multiclass.wu.test.out$statistic, statistic)
  expect_equal(multiclass.wu.test.out$parameter, df)
  expect_equal(multiclass.wu.test.out$p.value, pvalue)
  expect_equal(multiclass.wu.test.out$method, method)
  expect_equal(multiclass.wu.test.out$data.name, "y, x.1, x.2, x.3, x.4, x.5, x.6, x.7, and x.8")
})

test_that("test .multiclass.wu.test.formula.wide: data is attached logical data.frame, multiclass.wu.test.formula(y~x.1+x.2+x.3+x.4+x.5+x.6+x.7+x.8, data=test.df)", {
  test.data <- get.test.data.1()

  test.df <- data.frame(x = test.data$x, y = test.data$y)
  attach(test.df, warn.conflicts = FALSE)
  multiclass.wu.test.out <- multiclass.wu.test(y ~ x.1 + x.2 + x.3 + x.4 + x.5 + x.6 + x.7 + x.8)

  # expect_invisible(multiclass.wu.test.default(x, y))
  expect_length(multiclass.wu.test.out, 5)
  expect_equal(names(multiclass.wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(multiclass.wu.test.out$statistic, statistic)
  expect_equal(multiclass.wu.test.out$parameter, df)
  expect_equal(multiclass.wu.test.out$p.value, pvalue)
  expect_equal(multiclass.wu.test.out$method, method)
  expect_equal(multiclass.wu.test.out$data.name, "y, x.1, x.2, x.3, x.4, x.5, x.6, x.7, and x.8")
})

#########################
# Long format formula
#########################

test_that("test .multiclass.wu.test.formula.long: data.frame, y/x are logical, models and samples are factors", {
  test.data <- get.test.data.1()
  # suppressMessages(attach(test.data, warn.conflicts = FALSE))

  test.df <- data.frame(
    x = c(test.data$x),
    y = rep(test.data$y, dim(test.data$x)[2]),
    models = factor(col(test.data$x)),
    samples = factor(row(test.data$x))
  )


  multiclass.wu.test.out <- multiclass.wu.test(y ~ x:models | samples, data = test.df)

  # expect_invisible(multiclass.wu.test.default(x, y))
  expect_length(multiclass.wu.test.out, 5)
  expect_equal(names(multiclass.wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(multiclass.wu.test.out$statistic, statistic)
  expect_equal(multiclass.wu.test.out$parameter, df)
  expect_equal(multiclass.wu.test.out$p.value, pvalue)
  expect_equal(multiclass.wu.test.out$method, method)
  expect_equal(multiclass.wu.test.out$data.name, "y, x, models, and samples")
})

###########################
# Cross tabulated
###########################

test_that("test multiclass.wu.test.xtabs: data is cross tab", {
  test.data <- get.test.data.1()

  test.xt <- xtabs(~ y + ., data = data.frame(x = test.data$x, y = test.data$y))
  multiclass.wu.test.out <- multiclass.wu.test(test.xt)

  # expect_invisible(multiclass.wu.test.default(x, y))
  expect_length(multiclass.wu.test.out, 5)
  expect_equal(names(multiclass.wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(multiclass.wu.test.out$statistic, statistic)
  expect_equal(multiclass.wu.test.out$parameter, df)
  expect_equal(multiclass.wu.test.out$p.value, pvalue)
  expect_equal(multiclass.wu.test.out$method, method)
  # expect_equal(multiclass.wu.test.out$data.name, 'y and x')
})

test_that("test .multiclass.wu.test.formula.xtabs: data is cross tab, multiclass.wu.test.formula(Freq~.||y)", {
  test.data <- get.test.data.1()

  test.df <- data.frame(xtabs(~ y + ., data = data.frame(x = test.data$x, y = test.data$y)))
  multiclass.wu.test.out <- multiclass.wu.test(Freq ~ . || y, data = test.df)

  # expect_invisible(multiclass.wu.test.default(x, y))
  expect_length(multiclass.wu.test.out, 5)
  expect_equal(names(multiclass.wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(multiclass.wu.test.out$statistic, statistic)
  expect_equal(multiclass.wu.test.out$parameter, df)
  expect_equal(multiclass.wu.test.out$p.value, pvalue)
  expect_equal(multiclass.wu.test.out$method, method)
  # expect_equal(multiclass.wu.test.out$data.name, 'y and x')
})

#####################################
# Testing coronary.disease.tabulated
#####################################

test_that("test coronary.disease.tabulated: xtabs", {
  multiclass.wu.test.out <- multiclass.wu.test(xtabs(Freq ~ D + .,  data = coronary.disease.tabulated))

  statistic <- 811.3
  names(statistic) <- "chi-squared"
  pvalue <- 0
  df <- 4
  names(df) <- "df"

  # expect_invisible(multiclass.wu.test.default(x, y))
  expect_length(multiclass.wu.test.out, 5)
  expect_equal(names(multiclass.wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(multiclass.wu.test.out$statistic, statistic, tolerance = 1e-2)
  expect_equal(multiclass.wu.test.out$parameter, df)
  expect_equal(multiclass.wu.test.out$p.value, pvalue)
  expect_equal(multiclass.wu.test.out$method, "Multiclass Wu's test")
  # expect_equal(multiclass.wu.test.out$data.name, 'y and x')
})

test_that("test coronary.disease.tabulated: xtabs formula", {
  multiclass.wu.test.out <- multiclass.wu.test(Freq ~ . || D,  data = coronary.disease.tabulated)

  statistic <- 811.3
  names(statistic) <- "chi-squared"
  pvalue <- 0
  df <- 4
  names(df) <- "df"

  # expect_invisible(multiclass.wu.test.default(x, y))
  expect_length(multiclass.wu.test.out, 5)
  expect_equal(names(multiclass.wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(multiclass.wu.test.out$statistic, statistic, tolerance = 1e-2)
  expect_equal(multiclass.wu.test.out$parameter, df)
  expect_equal(multiclass.wu.test.out$p.value, pvalue)
  expect_equal(multiclass.wu.test.out$method, "Multiclass Wu's test")
  # expect_equal(multiclass.wu.test.out$data.name, 'y and x')
})

test_that("test coronary.disease.tabulated: xtabs formula", {
  multiclass.wu.test.out <- multiclass.wu.test(Freq ~ T1 + T2 + T3 || D,  data = coronary.disease.tabulated)

  statistic <- 811.3
  names(statistic) <- "chi-squared"
  pvalue <- 0
  df <- 4
  names(df) <- "df"

  # expect_invisible(multiclass.wu.test.default(x, y))
  expect_length(multiclass.wu.test.out, 5)
  expect_equal(names(multiclass.wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(multiclass.wu.test.out$statistic, statistic, tolerance = 1e-2)
  expect_equal(multiclass.wu.test.out$parameter, df)
  expect_equal(multiclass.wu.test.out$p.value, pvalue)
  expect_equal(multiclass.wu.test.out$method, "Multiclass Wu's test")
  # expect_equal(multiclass.wu.test.out$data.name, 'y and x')
})

test_that("test coronary.disease.wide: formula", {
  multiclass.wu.test.out <- multiclass.wu.test(D ~ .,  data = coronary.disease.wide)

  statistic <- 811.3
  names(statistic) <- "chi-squared"
  pvalue <- 0
  df <- 4
  names(df) <- "df"

  # expect_invisible(multiclass.wu.test.default(x, y))
  expect_length(multiclass.wu.test.out, 5)
  expect_equal(names(multiclass.wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(multiclass.wu.test.out$statistic, statistic, tolerance = 1e-2)
  expect_equal(multiclass.wu.test.out$parameter, df)
  expect_equal(multiclass.wu.test.out$p.value, pvalue)
  expect_equal(multiclass.wu.test.out$method, "Multiclass Wu's test")
  # expect_equal(multiclass.wu.test.out$data.name, 'y and x')
})

test_that("test coronary.disease.long: default", {
  multiclass.wu.test.out <- multiclass.wu.test(x=coronary.disease.long$x,
                         y=coronary.disease.long$D,
                         samples=coronary.disease.long$sample,
                         models=coronary.disease.long$model)

  statistic <- 811.3
  names(statistic) <- "chi-squared"
  pvalue <- 0
  df <- 4
  names(df) <- "df"

  # expect_invisible(multiclass.wu.test.default(x, y))
  expect_length(multiclass.wu.test.out, 5)
  expect_equal(names(multiclass.wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(multiclass.wu.test.out$statistic, statistic, tolerance = 1e-2)
  expect_equal(multiclass.wu.test.out$parameter, df)
  expect_equal(multiclass.wu.test.out$p.value, pvalue)
  expect_equal(multiclass.wu.test.out$method, "Multiclass Wu's test")
  # expect_equal(multiclass.wu.test.out$data.name, 'y and x')
})

test_that("test coronary.disease.long: formula", {
  multiclass.wu.test.out <- multiclass.wu.test(D ~ x:model|sample,  data = coronary.disease.long)

  statistic <- 811.3
  names(statistic) <- "chi-squared"
  pvalue <- 0
  df <- 4
  names(df) <- "df"

  # expect_invisible(multiclass.wu.test.default(x, y))
  expect_length(multiclass.wu.test.out, 5)
  expect_equal(names(multiclass.wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(multiclass.wu.test.out$statistic, statistic, tolerance = 1e-2)
  expect_equal(multiclass.wu.test.out$parameter, df)
  expect_equal(multiclass.wu.test.out$p.value, pvalue)
  expect_equal(multiclass.wu.test.out$method, "Multiclass Wu's test")
  # expect_equal(multiclass.wu.test.out$data.name, 'y and x')
})
