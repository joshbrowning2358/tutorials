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
mtcars[, c(newVarNames) := NULL] # Can easily delete all variables at once

## Referencing/extracting variables
mtcars[, mpg*10]
## Let's say you want to multiply several variables by 10
multiplyTheseVariables = c("mpg", "cyl", "disp", "hp", "drat")
for(name in multiplyTheseVariables){
    mtcars[, name * 10] # Doesn't work, no variable is called "name"
}
for(name in multiplyTheseVariables){
    mtcars[, get(name) * 10] # Instead, use get
}
# To actually see the results, use print
for(name in multiplyTheseVariables){
    print(name)
    print(mtcars[, get(name) * 10])
}

## Multiple Column Assignment
mtcars[, c("allZero", "allOne") := list(0, 1)]
mtcars[, c("newmpg", "newcyl") := list(mpg, cyl)]
oldNames = c("mpg", "cyl")
# Be careful if filtering, you'll need two filters!
mtcars[, c("newmpg", "newcyl") := mtcars[, oldNames, with = FALSE]]
mtcars[1:3, c("newmpg", "newcyl") := mtcars[, oldNames, with = FALSE]]
mtcars[1:3, c("newmpg", "newcyl") := mtcars[1:3, oldNames, with = FALSE]]
