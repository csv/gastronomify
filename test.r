# library(testthat)

# The mean recipe should be the base recipe.
recipe = c(foo = 2, bar = 1, baz = 1)
observed = gastronomify(
  x = c(2007, 2007, 2007, 2008, 2008, 2008),
  y = c(8, 9, 6, 2, 12, 5),
  group = factor(c('red', 'blue', 'green', 'red', 'blue',  'green')),
  recipe = recipe
)
colSums(observed[-1]) == recipe * 2

# This particular recipe should match.
expected = data.frame(
  x = c(2007, 2008),
  foo = c(1.714286, 2.285714),
  bar = c(1.0909091, 0.9090909),
  baz = c(1.6, 0.4)
)
colnames(expected)[-1] <- c('foo (blue)', 'bar (green)',  'baz (red)')
observed == expected
