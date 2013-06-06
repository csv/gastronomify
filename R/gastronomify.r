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
#' gastronomify(x = ChickWeight$Diet, y = ChickWeight$Weight, group = ChickWeight$Time,
#'   recipe = rnorm(length(unique(ChickWeight$Time)), mean = 10))
#' @return data.frame
gastronomify <- function(x, y, group, recipe, inflation = 10) {
  data = data.frame(
    x = x,
    y = y,
    group = group
  )

  data.normalized <- dcast(data, x ~ group, value.var = 'y')
  data.normalized[-1] <- data.frame(lapply(data.normalized[-1], function(vec) { vec / mean(vec) }))
  # data.normalized[-1] <- .inflate(data[-1], inflation)

  data.recipe <- ddply(data.normalized, 'x', function(df) {df[1,-1] * recipe})
  colnames(data.recipe)[-1] <- paste(names(recipe), ' (', colnames(data.recipe[-1]), ')', sep = '')

  data.recipe
}
