make_three_class_fixture <- function() {
  data.frame(
    y = factor(c("a", "a", "a", "b", "b", "b", "c", "c", "c")),
    x1 = factor(c("a", "b", "c", "b", "c", "a", "c", "a", "b")),
    x2 = factor(c("a", "c", "b", "c", "a", "b", "b", "c", "a")),
    x3 = factor(c("b", "a", "c", "a", "c", "b", "c", "b", "a"))
  )
}
