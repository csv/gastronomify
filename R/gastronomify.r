#' @docType package
#'
#' @import reshape2 plyr

# Inflate the variation
.inflate <- function(df, x) {
  1 + (x * (df - 1))
}

#' Turn some data into food.
#'
#' @param x a factor; each level of the factor will correspond to a recipe
#' @param y a numerical response variable, used to weight the ingredients
#' @param group a factor used to map data to ingredients
#' @param recipe named vector where each name is an ingredient and each
#'   element is its quantity; its length must be the same as the number of
#'   levels in `group`
#' @export
#' @examples
#' recipe =  c(apples = 3, bananas = 1, cherries = 12, grapes = 14,
#'   kiwis = 2, lemons = 0.5, mangos = 1, nectarines = 2, oranges = 2,
#'   pineapples = 0.5, raspberries = 8, watermelons = 0.25)
#' gastronomify(
#'   x = paste('Diet', ChickWeight$Diet),
#'   y = ChickWeight$weight,
#'   group = paste(ChickWeight$Time, 'days'),
#'   recipe = recipe)
#' @return data.frame
gastronomify <- function(x, y, group, recipe, inflation = 10) {
  #
  # Check inputs
  #

  # Recipe names
  if (is.null(names(recipe))) {
    stop('Recipe must have ingredient names, not just quantities.')
  }

  # Recipe length
  n <- length(levels(factor(group)))
  if (n >= 1) {
    truncated.recipe <- recipe[1:n]
    rm('recipe')
  } else {
    stop('Recipe must have at least one ingredient.')
  }

  data = data.frame(
    x = x,
    y = y,
    group = group
  )

  data.normalized <- dcast(data, x ~ group, value.var = 'y', fun.aggregate = mean)
  data.normalized[-1] <- data.frame(lapply(data.normalized[-1], function(vec) { vec / mean(vec) }))
  # data.normalized[-1] <- .inflate(data[-1], inflation)

  data.recipe <- ddply(data.normalized, 'x', function(df) {df[1,-1] * truncated.recipe})
  colnames(data.recipe)[-1] <- paste(names(truncated.recipe), ' (', colnames(data.recipe[-1]), ')', sep = '')

  # Remove the x column
  rownames(data.recipe) <- data.recipe[,1]
  data.recipe <- data.recipe[-1]
}
