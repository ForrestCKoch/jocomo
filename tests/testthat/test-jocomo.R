test_that('test jocomo.chisq.test.default on baron pancreas.predictions', {

  set.seed(42)
  .tmp <- pancreas.predictions |> subset(train=='baron')

  jocomo.out <- with(.tmp, jocomo.chisq.test.default(pred, label, sample, method, test, correct=T, warn=F))

  statistic <- 2989.5
  names(statistic) <- "chi-squared"
  pvalue <- 0
  df <- 190
  names(df) <- "df"

  # expect_invisible(multiclass.wu.test.default(x, y))
  expect_length(jocomo.out, 5)
  expect_equal(names(jocomo.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(jocomo.out$statistic, statistic, tolerance = 1e-2)
  expect_equal(jocomo.out$parameter, df)
  expect_equal(jocomo.out$p.value, pvalue)
  expect_equal(jocomo.out$method, "Chi-squared Test for the Joint Comparison of Multiple Models")
})

test_that('test jocomo.chisq.test (default) on baron pancreas.predictions', {

  set.seed(42)
  .tmp <- pancreas.predictions |> subset(train=='baron')

  jocomo.out <- with(.tmp, jocomo.chisq.test(pred, label, sample, method, test, correct=T, warn=F))

  statistic <- 2989.5
  names(statistic) <- "chi-squared"
  pvalue <- 0
  df <- 190
  names(df) <- "df"

  # expect_invisible(multiclass.wu.test.default(x, y))
  expect_length(jocomo.out, 5)
  expect_equal(names(jocomo.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(jocomo.out$statistic, statistic, tolerance = 1e-2)
  expect_equal(jocomo.out$parameter, df)
  expect_equal(jocomo.out$p.value, pvalue)
  expect_equal(jocomo.out$method, "Chi-squared Test for the Joint Comparison of Multiple Models")
})

test_that('test jocomo.chisq.test.formula (long) on baron pancreas.predictions', {

  set.seed(42)
  .tmp <- pancreas.predictions |> subset(train=='baron')

  jocomo.out <- jocomo.chisq.test.formula(label~pred:method|sample|test, data=.tmp, correct=T, warn=F)

  statistic <- 2989.5
  names(statistic) <- "chi-squared"
  pvalue <- 0
  df <- 190
  names(df) <- "df"

  # expect_invisible(multiclass.wu.test.default(x, y))
  expect_length(jocomo.out, 5)
  expect_equal(names(jocomo.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(jocomo.out$statistic, statistic, tolerance = 1e-2)
  expect_equal(jocomo.out$parameter, df)
  expect_equal(jocomo.out$p.value, pvalue)
  expect_equal(jocomo.out$method, "Chi-squared Test for the Joint Comparison of Multiple Models")
})

test_that('test jocomo.chisq.test (formula.long) on baron pancreas.predictions', {

  set.seed(42)
  .tmp <- pancreas.predictions |> subset(train=='baron')

  jocomo.out <- jocomo.chisq.test(label~pred:method|sample|test, data=.tmp, correct=T, warn=F)

  statistic <- 2989.5
  names(statistic) <- "chi-squared"
  pvalue <- 0
  df <- 190
  names(df) <- "df"

  # expect_invisible(multiclass.wu.test.default(x, y))
  expect_length(jocomo.out, 5)
  expect_equal(names(jocomo.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(jocomo.out$statistic, statistic, tolerance = 1e-2)
  expect_equal(jocomo.out$parameter, df)
  expect_equal(jocomo.out$p.value, pvalue)
  expect_equal(jocomo.out$method, "Chi-squared Test for the Joint Comparison of Multiple Models")
})

test_that('test jocomo.chisq.test.formula (wide) on baron pancreas.predictions', {

  set.seed(42)
  .tmp <- pancreas.predictions |>
    subset(train=='baron', c('label', 'pred', 'method', 'sample', 'test')) |>
    reshape(direction='wide', idvar=c('label', 'sample', 'test'), timevar='method')
  names(.tmp) <- gsub('pred.', '', names(.tmp))

  jocomo.out <- jocomo.chisq.test.formula(label~ACTINN + moana + scID + scmap + singlecellNet + singleR|sample|test, data=.tmp, correct=T, warn=F)

  statistic <- 17951.5
  names(statistic) <- "chi-squared"
  pvalue <- 0
  df <- 190
  names(df) <- "df"

  # expect_invisible(multiclass.wu.test.default(x, y))
  expect_length(jocomo.out, 5)
  expect_equal(names(jocomo.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(jocomo.out$statistic, statistic, tolerance = 1e-2)
  expect_equal(jocomo.out$parameter, df)
  expect_equal(jocomo.out$p.value, pvalue)
  expect_equal(jocomo.out$method, "Chi-squared Test for the Joint Comparison of Multiple Models")
})

test_that('test jocomo.chisq.test.formula (wide, dot notation) on baron pancreas.predictions', {

  set.seed(42)
  .tmp <- pancreas.predictions |>
    subset(train=='baron', c('label', 'pred', 'method', 'sample', 'test')) |>
    reshape(direction='wide', idvar=c('label', 'sample', 'test'), timevar='method')

  jocomo.out <- jocomo.chisq.test.formula(label~.|sample|test, data=.tmp, correct=T, warn=F)

  statistic <- 17951.5
  names(statistic) <- "chi-squared"
  pvalue <- 0
  df <- 190
  names(df) <- "df"

  # expect_invisible(multiclass.wu.test.default(x, y))
  expect_length(jocomo.out, 5)
  expect_equal(names(jocomo.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(jocomo.out$statistic, statistic, tolerance = 1e-2)
  expect_equal(jocomo.out$parameter, df)
  expect_equal(jocomo.out$p.value, pvalue)
  expect_equal(jocomo.out$method, "Chi-squared Test for the Joint Comparison of Multiple Models")
})

test_that("grouped chi-squared validation rejects malformed structures", {
  x <- matrix(c("a", "b", "a", "b", "a", "b", "a", "b"),
              nrow = 4L, ncol = 2L)
  y <- factor(c("a", "a", "b", "b"))

  expect_error(
    jocomo.chisq.test(x, y, groups = c("g1", "g1", "g2", "g2"),
                      correct = TRUE),
    "at least two observed outcome levels"
  )
  expect_error(
    jocomo.chisq.test(
      x = c(x),
      y = rep(y, 2L),
      samples = rep(seq_len(4L), 2L),
      models = c(col(x)),
      groups = c("g1", "g1", "g1", "g2", "g2", "g2", "g2", "g2"),
      correct = TRUE
    ),
    "exactly one prediction"
  )
  expect_error(
    jocomo.chisq.test(x, y, groups = c("g1", NA, "g1", "g1")),
    "must not contain missing values"
  )
})
