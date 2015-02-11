## data.table tutorial

library(data.table)

## Get an example data.table from mtcars
mtcars$model = rownames(mtcars)
mtcars = data.table(mtcars)

## Assign new variables
mtcars[, kpg := mpg / 0.6] # Kilometers / gallon
mtcars[, kpg := round(mpg / 0.6, 1)]
newVarNames = paste0("copyMPG", 1:10)
mtcars[, newVarNames := mpg] # Doesn't work!
mtcars[, newVarNames := NULL] # Delete mistake
mtcars[, c(newVarNames) := mpg] # Have to wrap with a c()
