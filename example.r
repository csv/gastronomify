library(gastronomify)

fruit.salad  <-  c(apples = 3, bananas = 1, cherries = 12, grapes = 14,
                   kiwis = 2, lemons = 0.5, mangos = 1, nectarines = 2, oranges = 2,
                   pineapples = 0.5, raspberries = 8, watermelons = 0.25)

data.fruit.salad <- gastronomify(
  x = paste('Diet', ChickWeight$Diet),
  y = ChickWeight$weight,
  group = paste(ChickWeight$Time, 'days'),
  recipe = fruit.salad,
  inflation = 2
)

food.names <- sapply(colnames(data.fruit.salad), function(name) { strsplit(name, ' ')[[1]][1] })
recipes <- sapply(rownames(data.fruit.salad), function(diet) {
  this.df <- t(data.fruit.salad[diet,])
  rownames(this.df) <- food.names
  recipe <- paste(capture.output(print(this.df)), collapse = '\n')
  recipe
})

description <- paste(
  paste('There are', nrow(data.fruit.salad), 'fruit salad recipes below. '),
  'They are named\n',
  paste('* ', rownames(data.fruit.salad), '\n', collapse = ''), '\n',
  'Please make each recipe in a separate bowl.\n',
  'Label each bowl according to its recipe,\n',
  'and deliver the food to the specified place and time.\n',
  'The recipes follow.\n\n',
  paste(recipes, collapse = '\n\n\n'), '\n',
  sep = ''
)

taskrabbit(
  email = Sys.getenv('TASKRABBIT_EMAIL'),
  password = Sys.getenv('TASKRABBIT_PASSWORD'),
  price,
  freeform.address, lng, lat,
  name, description, datetime = Sys.time()
)
