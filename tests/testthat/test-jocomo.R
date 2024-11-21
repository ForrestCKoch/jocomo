test_that('test jocomo.chisq.test.default on baron pancreas.predictions', {

  set.seed(42)
  .tmp <- pancreas.predictions |> subset(train=='baron')

  jocomo.out <- with(.tmp, jocomo.chisq.test.default(pred, label, sample, method, test, correct=T, warn=F))

  statistic <- 851.44
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

  statistic <- 851.44
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

  statistic <- 851.44
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

  statistic <- 851.44
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

  statistic <- 851.44
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

  statistic <- 851.44
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
