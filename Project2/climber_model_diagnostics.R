library('car');
library('olsrr');
library('rgl');

# MA4710 Project Peer Reviews: Regression Diagnostics
# Each labeled section corresponds to the programming assignment requirements
#
# @author Daniel Henderson
# Date Last Modified: 6/1/20

# Part 0 - Parse data
data <- read.csv("NumericData.csv")
n <- nrow(data)
data$X <- NULL      # removes a col that shouldn't be in the data frame


# Part 1: determine projection matrix
X <- with(data, cbind(1, Height, Weight, ApeIndex, YearsClimbing, MaxPullUpReps))
P_hat <- X %*% solve(crossprod(X)) %*% t(X)
leverage <- sort(diag(P_hat))
message('leverage Points\n')
leverage
# Part 2
model <- lm(HardestGrade ~ Height + Weight + ApeIndex + YearsClimbing +
    MaxPullUpReps,data)
scatterplotMatrix(~ HardestGrade + Height + Weight + ApeIndex + YearsClimbing
    + MaxPullUpReps, data, smooth=FALSE)

# Part 3
res_std <- rstandard(model)
qqnorm(res_std)
qqline(res_std)
shapiro.test(res_std)

# Part 4 and 5
residualPlots(model, id=list(n=4), quadratic=FALSE, type='rstandard')

# Part 6
plot(model, which=3)    # location-spread

# Part 7
plot(model, which=5)    # residual-leverage

# Part 9
plot(model, which=4)    # residual-leverage

# Part 10
ols_plot_dffits(model)

# Part 10
ols_plot_dfbetas(model)

# Part 11
crPlots(model, id=TRUE)
