jocomo.test <- function(...) UseMethod("jocomo.test")

#' Title
#'
#' @param x
#' @param y
#' @param subjects
#' @param models
#' @param folds
#'
#' @return
#' @export
#'
#' @examples
jocomo.test.default <- function(x, y, subjects, models, folds) {}

#' Title
#'
#' @param formula
#' @param data
#'
#' @return
#' @export
#'
#' @examples
jocomo.test.formula <- function(formula, data=parent.frame()) {}

.jocomo.test.formula.long <- function(formula, data){}

.jocomo.test.formula.wide <- function(formula, data){}

.jocomo.test.formula.xtabs <- function(formula, data){}

#' Title
#'
#' @param xt
#' @param data
#'
#' @return
#' @export
#'
#' @examples
jocomo.test.xtabs <- function(xt, data=parent.frame()) {}
