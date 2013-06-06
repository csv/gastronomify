
# Inflate the variation
.inflate <- function(df, x) {
  1 + (x * (df - 1))
}

#' Turn a data frame into guacamole.
#'
#' This is a wrapper around \code{\link{gastronomify}}
#' that uses the guacamole recipe from
#' http://www.theyummylife.com/guacamole
#'
#' @param x 
#' @param ... expressions evaluated in the context of \code{df} and 
#'   then fed to \code{\link{order}}
#' @keywords manip
#' @export
#' @examples
#' mtcars[with(mtcars, order(cyl, disp)), ]
#' arrange(mtcars, cyl, disp)
#' arrange(mtcars, cyl, desc(disp))
guacamole <- function(x, y, z, data, inflation = 10) {
  recipe <- c(
    avocados =             4,   # ripe medium-size avocados; Haas recommended, if available
    garlic.powder.tsp =    1/2, # teaspoon garlic powder
    kosher.salt.tsp =      1/2, # teaspoon kosher salt
    black.pepper.tsp =     1/2, # teaspoon freshly ground black pepper
    # OPTIONAL ADDITIONS:
    lime.juice.tbsp =      1,   # tablespoon fresh lime juice
    chopped.cilantro.cup = 1/4  # cup chopped cilantro
         # hot green chili (2 serranos or 2-3 jalapenos), finely diced
  # 1/2, # small white onion, finely diced
  # 1    # medium tomato, deseeded & chopped in 1/4" pieces
  )
  
  yearly <- dcast(
    ddply(math.tests, c('Year', 'Grade'), function(df) {
      c(mean=mean(df$Mean.Scale.Score),sd=sd(df$Mean.Scale.Score))
    }),
    Year ~ Grade, value.var = 'mean')
  rownames(yearly) <- yearly$Year
  grades <- c('3','4','5','6','7','8')
  
  yearly.normalized <- yearly
  yearly.normalized[grades] <- data.frame(lapply(yearly[grades], function(vec) { vec / mean(vec) }))
  yearly.normalized[grades] <- .inflate(yearly.normalized[grades], INFLATION)
  
  # This is a silly way of doing it, but it works.
  data.guacamole <- ddply(yearly.normalized, 'Year', function(df) {df[1,grades] * guacamole})
  colnames(data.guacamole)[-1] <- paste(names(guacamole), ' (Grade ', grades, ')', sep = '')
  
  print('Data-driven guacamole recipes')
  print('Each recipe represents the average tests scores by grade for a particular year.')
  print(round(data.guacamole, 2))
  write.csv(round(data.guacamole, 2), 'data_guacamole.csv', row.names = F)
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
