library(testthat)
library(plyr)
library(reshape2)
source('../R/gastronomify.r')

test_that('The result should be structured properly', {
  recipe = c(foo = 2, bar = 1, baz = 1)
  observed = gastronomify(
    x = c(2007, 2007, 2007, 2008, 2008, 2008),
    y = c(8, 9, 6, 2, 12, 5),
    group = factor(c('red', 'blue', 'green', 'red', 'blue',  'green')),
    recipe = recipe
  )
  expect_that(dim(observed), equals(c(2, 3)))
  expect_that(rownames(observed), equals(2007:2008))
  expect_that(colnames(observed), equals(c('foo (red)', 'bar (blue)', 'baz (green)')))
})

test_that('The mean recipe should be the base recipe.', {
  recipe = c(foo = 2, bar = 1, baz = 1)
  observed = gastronomify(
    x = c(2007, 2007, 2007, 2008, 2008, 2008),
    y = c(8, 9, 6, 2, 12, 5),
    group = factor(c('red', 'blue', 'green', 'red', 'blue',  'green')),
    recipe = recipe
  )
  expect_that(as.vector(colSums(observed[-1])), equals(as.vector(recipe * 2)))
})

test_that('This particular recipe should match.', {
  recipe = c(foo = 2, bar = 1, baz = 1)
  observed = gastronomify(
    x = c(2007, 2007, 2007, 2008, 2008, 2008),
    y = c(8, 9, 6, 2, 12, 5),
    group = factor(c('red', 'blue', 'green', 'red', 'blue',  'green')),
    recipe = recipe
  )
  expected = data.frame(
    x = c(2007, 2008),
    foo = c(1.714286, 2.285714),
    bar = c(1.0909091, 0.9090909),
    baz = c(1.6, 0.4)
  )
  colnames(expected)[-1] <- c('foo (blue)', 'bar (green)',  'baz (red)')
  expect_that(observed[1], equals(expected[1]))
  expect_that(round(observed[-1], -5), equals(round(expected[-1], -5)))
})

test_that('Fruit salad should work.', {
  recipe =  c(apples = 3, bananas = 1, cherries = 12, grapes = 14,
    kiwis = 2, lemons = 0.5, mangos = 1, nectarines = 2, oranges = 2,
    pineapples = 0.5, raspberries = 8, watermelons = 0.25)
  observed = gastronomify(
    x = paste('Diet', ChickWeight$Diet),
    y = ChickWeight$weight,
    group = paste(ChickWeight$Time, 'days'),
    recipe = recipe)
  expect_that(as.vector(colMeans(observed[-1])), equals(as.vector(recipe)))
})

test_that('If there are more items in the recipe than levels, truncate the recipe.', {
  a <- gastronomify(x = paste('vs', mtcars$vs), y = mtcars$mpg, group = paste('am', mtcars$am), recipe = c(flour = 2, water = 3))
  b <- gastronomify(x = paste('vs', mtcars$vs), y = mtcars$mpg, group = paste('am', mtcars$am), recipe = c(flour = 2, water = 3, oil = 0.5))
  expect_that(a, equals(b))
})
