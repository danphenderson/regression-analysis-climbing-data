# This script investigates possible transformations to my linear model
# to better meet the assumptions.
# We start with last weeks model while changing years_climbing as numeric
# with the hope of uncovering potential interactions and simply interpreation.
#
# Date Last Modified: 6/18/20
# @author Daniel Henderson

suppressMessages(library('car'))
data <- read.csv("FullData.csv")
data$sex <- as.factor(data$sex)

# Base model and some diagnostics
base_model <- lm(grade ~ years_climbing + hangboard_freq + sex*bmi + max_pull, data)
summary(base_model)
hist(rstandard(base_model))
qqPlot(base_model, main="Base Model")
message("Breusch-Pagan Test on Base Model\n")
ncvTest(base_model)
shapiro.test(rstandard(base_model))
residualPlots(base_model, main="Base Model", fitted=FALSE)
plot(base_model, which=1, main="Base Model")
plot(base_model, which=3, main="Base Model")

# Revist initail data exploration
scatterplotMatrix(~ grade + height + weight + ape_index + years_climbing +
    climbing_sessions + campusboard_freq + hangboard_freq + hours_campusboard +
    endurance_frequency + strength_freq + hours_strength + max_pull
    + max_push + bmi | sex,
    data, regLine=FALSE, smooth=FALSE
)

# Add climbing sessions as a predictor
working_model <- update(base_model, ~ . + climbing_sessions)
anova(base_model, working_model)
message("Breusch-Pagan Test on Base Model Plus Climbing Sessions\n")
ncvTest(working_model)
residualPlots(working_model, main="Base Model Plus Climbing Sessions", fitted=FALSE)
plot(working_model, main="Base Model Plus Climbing Sessions")

# Investigation of Max Pull Transformation
invTranPlot(grade ~ max_pull, data, main="Guide for Max Pull Transformation")
temp_model <- update(working_model, ~ . - max_pull + log(max_pull))
summary(temp_model)
plot(temp_model, main="Working Model (Transformation of Max Pull)")
residualPlots(temp_model, main="Working Model (Transformation of Max Pull)")
working_model <- temp_model

# Investigation of Years Climbing Transformation
invTranPlot(grade ~ I(years_climbing+1), data, main="Guide for Years Climbing Transformation")
temp_model <- update(working_model, ~ . - years_climbing + log(years_climbing+1))
residualPlots(temp_model, main="Working Model (Transformation of Years Climbing)")
working_model <- temp_model

# final model plots
plot(working_model, main="Transformed Model")
qqPlot(working_model, main="Transformed Model")
message("Breusch-Pagan Test on Final Model\n")
ncvTest(working_model)
shapiro.test(rstandard(working_model))
