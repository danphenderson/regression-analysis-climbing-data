library(car)
library(leaps)
library(pls)

data <- read.csv('FullData.csv')
data$sex <- as.factor(data$sex)
data$years_climbing_tran <- log(data$years_climbing + 1)
data$max_pull_tran <- log(data$max_pull)
data$max_push_tran <- 1/sqrt(data$max_push)

data_rs <- regsubsets(grade ~ ., data)
plot(data_rs) # BIC
plot(data_rs, scale='adjr2') # Adjusted R^2
plot(data_rs, scale='Cp') # Mallows Cp
