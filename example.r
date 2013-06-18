fruit.salad  <-  c(apples = 3, bananas = 1, cherries = 12, grapes = 14,
                   kiwis = 2, lemons = 0.5, mangos = 1, nectarines = 2, oranges = 2,
                   pineapples = 0.5, raspberries = 8, watermelons = 0.25)

data.fruit.salad <- gastronomify(
  x = paste('Diet', ChickWeight$Diet),
  y = ChickWeight$weight,
  group = paste(ChickWeight$Time, 'days'),
  recipe = fruit.salad)

food.names <- sapply(colnames(data.fruit.salad), function(name) { strsplit(name, ' ')[[1]][1] })
recipes <- sapply(rownames(data.fruit.salad), function(diet) {
  this.df <- t(data.fruit.salad[diet,])
  rownames(this.df) <- food.names
  recipe <- paste(capture.output(print(this.df)), collapse = '\n')
  recipe
})




# taskrabbit(
#   email = Sys.getenv('TASKRABBIT_EMAIL'),
#   password = Sys.getenv('TASKRABBIT_PASSWORD'),
#   price,
#   freeform.address, lng, lat,
#   name, description, datetime = Sys.time()
# )
