library(ggplot2)
library(reshape2)
library(plyr)
library(gastronomify)

fruit.salad  <-  c(apples = 3, bananas = 1, cherries = 12, grapes = 14,
                   kiwis = 2, lemons = 0.5, mangos = 1, nectarines = 2, oranges = 2,
                   pineapples = 0.5, raspberries = 8, watermelons = 0.25)

# Tidy the dataset
vars <- c('Hair','Eye','Sex')
ddply(melt(data('HairEyeColor')), vars, function(df) {
  df[rep(1,df[1,'value']),vars]
})

ggplot(iris) + aes(meat = Hair, guacamole = Sex) + geom_taco()
