# test_that("multiplication works", {
#   expect_equal(2 * 2, 4)
# })

get.test.data.1 <- function(){
  y <- 1881209709 |> intToBits() |> as.logical()
  x <- c(1512222934, 2068776800, 522499376, 975421075,
         376266222, 1728131073, 499928810, 511640686) |>
    intToBits() |> as.logical() |> matrix(nrow=length(y))

  statistic <- 15.018873
  names(statistic) <- "Wu chi-squared"
  pvalue <- 0.37686577
  df <- 14
  names(df) <- "df"
  list(x=x, y=y, statistic=statistic, pvalue=pvalue, df=df, method="Wu's test")
}

test_that("test wu.statistic: stable",{
  test.data <- get.test.data.1()
  suppressMessages(attach(test.data))
  stat.out <- wu.statistic(x, y)
  names(stat.out) <- c("Wu chi-squared")
  expect_equal(stat.out, statistic)
})

test_that("test wu.test.default: data is logical, x is matrix",{
  test.data <- get.test.data.1()
  suppressMessages(attach(test.data))

  wu.test.out <- wu.test.default(x, y)

  #expect_invisible(wu.test.default(x, y))
  expect_length(wu.test.out, 5)
  expect_equal(names(wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(wu.test.out$statistic, statistic)
  expect_equal(wu.test.out$parameter, df)
  expect_equal(wu.test.out$p.value, pvalue)
  expect_equal(wu.test.out$method, method)
  expect_equal(wu.test.out$data.name, 'x and y')
})

test_that("test wu.test.default: data is numeric, x is matrix",{
  test.data <- get.test.data.1()
  suppressMessages(attach(test.data))
  x <- matrix(as.numeric(c(x)), nrow=length(y))
  y <- as.numeric(y)

  wu.test.out <- wu.test.default(x, y)

  #expect_invisible(wu.test.default(x, y))
  expect_length(wu.test.out, 5)
  expect_equal(names(wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(wu.test.out$statistic, statistic)
  expect_equal(wu.test.out$parameter, df)
  expect_equal(wu.test.out$p.value, pvalue)
  expect_equal(wu.test.out$method, method)
  expect_equal(wu.test.out$data.name, 'x and y')
})

test_that("test wu.test.default: data is character, x is matrix",{
  test.data <- get.test.data.1()
  suppressMessages(attach(test.data))
  x <- matrix(as.character(c(x)), nrow=length(y))
  y <- as.character(y)

  wu.test.out <- wu.test.default(x, y)

  #expect_invisible(wu.test.default(x, y))
  expect_length(wu.test.out, 5)
  expect_equal(names(wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(wu.test.out$statistic, statistic)
  expect_equal(wu.test.out$parameter, df)
  expect_equal(wu.test.out$p.value, pvalue)
  expect_equal(wu.test.out$method, method)
  expect_equal(wu.test.out$data.name, 'x and y')
})

test_that("test wu.test.default: data is logical, x is vector, models and subjects are factors",{
  test.data <- get.test.data.1()
  suppressMessages(attach(test.data))
  models <- factor(col(x))
  subjects <- factor(row(x))
  y <- rep(y,dim(x)[2])
  x <- c(x)


  wu.test.out <- wu.test.default(x, y, models=models, subjects=subjects)

  #expect_invisible(wu.test.default(x, y))
  expect_length(wu.test.out, 5)
  expect_equal(names(wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(wu.test.out$statistic, statistic)
  expect_equal(wu.test.out$parameter, df)
  expect_equal(wu.test.out$p.value, pvalue)
  expect_equal(wu.test.out$method, method)
  expect_equal(wu.test.out$data.name, 'x and y and models and subjects')
})

test_that("test wu.test.default: data is logical, x is vector, models and subjects are characters",{
  test.data <- get.test.data.1()
  suppressMessages(attach(test.data))
  models <- factor(col(x)) |> as.character()
  subjects <- factor(row(x)) |> as.character()
  y <- rep(y,dim(x)[2])
  x <- c(x)


  wu.test.out <- wu.test.default(x, y, models=models, subjects=subjects)

  #expect_invisible(wu.test.default(x, y))
  expect_length(wu.test.out, 5)
  expect_equal(names(wu.test.out), c("statistic", "parameter", "p.value", "method", "data.name"))
  expect_equal(wu.test.out$statistic, statistic)
  expect_equal(wu.test.out$parameter, df)
  expect_equal(wu.test.out$p.value, pvalue)
  expect_equal(wu.test.out$method, method)
  expect_equal(wu.test.out$data.name, 'x and y and models and subjects')
})
