library('car')
library('olsrr')
library('multcomp')

data <- read.csv("FullData.csv")
data$years_climbing <- as.factor(data$years_climbing)
data$sex <- as.factor(data$sex)

# model with all pairwise iteraction terms - Lots of NA's from years_climbing
model <- lm(grade ~ (sex + bmi + years_climbing + hangboard_freq + max_pull)^2, data)
summary(model)

# reduced model - removed all iteraction with years_climbing and other terms
model <- update(model,  ~ years_climbing + (sex + bmi + max_pull+ hangboard_freq)^2)
summary(model)

# further reduced - coef of bmi:max_pull p-value was 0.607
model <- update(model, ~. -bmi:max_pull)
summary(model)

# further reduced - coef of max_pull:hangboard_freq p-value was 0.339
model <- update(model, ~. -max_pull:hangboard_freq)
summary(model)

# further reduced - coef of bmi:hangboard_freq p-value was 0.316
model <- update(model, ~. -bmi:hangboard_freq)
summary(model)

# further reduced - coef of sexMale:hangboard_freq p-value was 0.176
model <- update(model, ~. -sex:hangboard_freq)
summary(model)

# further reduced - coef of sex:max_pull p-value was 0.404
model <- update(model, ~. -sex:max_pull)
summary(model)

# model after systematic reduction is:
#     grade ~ hangboard_freq + years_climbing + max_pull + sex*bmi

# normality plots - suggest assumptions hold
plot(model, which=2)
hist(rstandard(model), probability=TRUE)
shapiro.test(rstandard(model))

# linearity and homoscedascity investigation - holds when no pattern present
residualPlots(model)
plot(model, which=3) # scale-location, used for honmoscedascity investigation

# influence investigation
ols_plot_resid_lev(model)
ols_plot_dfbetas(model)
ols_plot_dffits(model)
ols_plot_hadi(model)

# Combining categories of years_climbing levels betwen 5.5 - 15+
data$years_climbing2 <- ifelse(as.numeric(data$years_climbing)>5, 5.5, as.numeric(data$years_climbing))
anova(model, update(model, ~. - years_climbing + as.factor(years_climbing2)))
