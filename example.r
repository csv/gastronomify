library(ggplot2)
library(reshape2)
library(plyr)
library(gastronomify)

# Tidy the dataset
vars <- c('Hair','Eye','Sex')
people <- ddply(melt(data('HairEyeColor')), vars, function(df) {
  df[rep(1,df[1,'value']),vars]
})

# Plots
ggplot(hair) + aes(meat = Hair, guacamole = Sex) + geom_taco()
ggplot(hair) + aes(meat = Eye, cheese = Hair, toasted = Sex) + geom_sandwich()

# x gets mapped to sandwich length,
# and y gets mapped to the number
# of pieces of the main filling.
ggplot(iris) + aes(x = Sepal.Length, y = Sepal.Width, meat = Species)

iris.premelt <- iris
iris.premelt$id <- rownames(iris)
iris.molten <- melt(iris.premelt, id.vars = c('Species','id'))

# x gets mapped to the number of items
# and scaled such that there are no more
# than four of the same item.
ggplot(iris.molten) +
  aes(group = Species, item = Species, x = value) +
  geom_dimsum()
