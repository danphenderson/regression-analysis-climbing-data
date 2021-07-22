# This script investigates the space of my climbing data set, to better
# understand issues of collinearity and address them appropriately.
# Then we perform an exhaustive search for the best model amongst my data
#
# MA4710 Spring 2020
# Date Last Modified: 6/27/19
# @author Daniel Henderson
library(car)
library(leaps)
library(pls)

# Below we parse in data and apply transformations before we investigate
# colinearity - the transformations are determined via /lambda value of
# invTransPlot() w/out getting carried away.
data <- read.csv('FullData.csv')
data$sex <- as.factor(data$sex)
data$years_climbing <- log(data$years_climbing + 1)
data$max_pull <- log(data$max_pull)
data$max_push <- 1/sqrt(data$max_push)

full_model  <- lm(grade ~ ., data)
null_model <- lm(grade ~ 1, data)

vif(full_model ) # all below 5 - max 3.6

# determine condition indices
data_prcomp <- prcomp(~ . - grade - sex, data, scale=TRUE)
kappa <- max(data_prcomp$sdev[1] / data_prcomp$sdev) # 4.3
kappa

# perform principle component regression
data_pcr <- pcr(grade ~ ., data=data, scale=TRUE)
summary(data_pcr)

# conject that this is the strongest correlated pair
cov(data$campusboard_freq, data$hours_campusboard)


# obtain best p+1-variable subsets w/ option of bmi:sex interaction
data_rs <- regsubsets(grade ~ . + bmi*sex, data)
plot(data_rs) # BIC
plot(data_rs, scale='adjr2') # Adjusted R^2
plot(data_rs, scale='Cp') # Mallows Cp

# update full_model so interaction can be choosen
full_model <- update(full_model, grade ~. + bmi*sex) #

# perform the stepwise selection procedure
step(null_model, scope=formula(full_model), direction='both')
# perform forward selection
step(null_model, scope=formula(full_model), direction='forward')
# perform backward elimination procedure
step(null_model, scope=formula(full_model), direction='backward')

final_model <- lm(grade ~ sex*bmi + max_pull + endurance_freq + hangboard_freq + climbing_sessions + years_climbing, data)
summary(final_model)

# Diagnostics
plot(final_model, main="final model")
residualPlots(final_model, main="final model")
message("Breusch-Pagan Test on Final Model\n")
ncvTest(final_model)
shapiro.test(rstandard(final_model))
