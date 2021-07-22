# MA4710 Project: Submission 1
# Each part in this file corresponds to an item in the list of required
# computations as specified in the assignment description.
#
# @author: Daniel Henderson
# Data Last Modified: 5/30/20

# Part 0: ----------------------------------------------------------------------
# Parse Data
data <- read.csv("NumericData.csv")
n <- nrow(data)
data$X <- NULL      # removes a col that shouldn't be in the data frame

# Part 1: ----------------------------------------------------------------------
# Define the LS - Problem
X <- with(data, cbind(1, Height, Weight, ApeIndex, YearsClimbing, MaxPullUpReps))
Y <- data$HardestGrade
colnames(X)[1] <- "Intercept"

# Part 2 -----------------------------------------------------------------------
beta_hat <- solve(crossprod(X), crossprod(X,Y))

# Part 3 -----------------------------------------------------------------------
y_hat <- X %*% beta_hat               # orthogonal projection of Y onto Col(X)
residual <- Y - y_hat

# Part 4 -----------------------------------------------------------------------
SSE <- crossprod(residual)            # square of euclidean-norm of residual
deg_freedom <- nrow(X) - ncol(X)
sigma_squared <- SSE / deg_freedom
SST <- (n-1)*var(Y)                   # total sum of squares (SST = SSE + SSR)

# Part 5 -----------------------------------------------------------------------
R <- cor(Y,y_hat)
R_squared <- 1 - SSE/SST
R_squared_adj <- 1 - (SSE/deg_freedom) / (SST/(n-1))

# Part 6 -----------------------------------------------------------------------
F_stat <- ((SST - SSE) / 5) / (SSE/deg_freedom)
F_stat_p_value <- pf(F_stat, 5, deg_freedom, lower.tail=FALSE)

# Part 7 -----------------------------------------------------------------------
cov_beta_hat <- solve(crossprod(X))    # matrix
var_beta_hat <- diag(cov_beta_hat) * as.vector(sigma_squared)
SE_beta <- sqrt(var_beta_hat)          # vector

# Part 8 -----------------------------------------------------------------------
t_stats <- beta_hat / SE_beta
t_stats_p_values <- 2 * pt(abs(t_stats), deg_freedom, lower.tail = FALSE)

# Part 9 -----------------------------------------------------------------------
prediction_data <- read.csv("predictionData.csv")
new_climbers <- with(prediction_data, cbind(1, Height, Weight, ApeIndex,
    YearsClimbing, MaxPullUpReps))
predicted_grades <- new_climbers %*% beta_hat
predicted_grades <- cbind(predicted_grades,
    predicted_grades - rep(1,nrow(predicted_grades))*as.vector(sqrt(sigma_squared)),
    predicted_grades + rep(1,nrow(predicted_grades))*as.vector(sqrt(sigma_squared))
)
colnames(predicted_grades) <- c("Point Estimate","Lower Bound", "Upper Bound")
rownames(predicted_grades) <- prediction_data$Name

# Printed results of Matrix algebra --------------------------------------------
message("\nModel Coefficients:")
cat(beta_hat,"\n")
message("\nResidual degrees of Freedom:\n", deg_freedom)
message("\nSSE:\n", SSE)
message("\nSigma Squared:\n", sigma_squared)
message("\nMultiple Correlation Coefficient (R):\n", R)
message("\nCorrelation Coefficient of Determination (R^2):\n", R_squared)
message("\nAdjusted Correlation Coefficient of Determination:\n", R_squared_adj)
message("\nF-Statistic and associated p-value in test of overall regression:")
cat(F_stat, F_stat_p_value,"\n")
message("\nt-statistic and p-value of each coefficient in model:")
cbind(t_stats, t_stats_p_values)
message("\nExtra observations:")
show(prediction_data)
message("\nPredicted hardest Grade:")
show(predicted_grades)


# Hypothesis Tests using lm and anova ------------------------------------------
model <- lm(HardestGrade ~ Height + Weight + ApeIndex + YearsClimbing +
    MaxPullUpReps,data)

# Part 10 ----------------------------------------------------------------------
message("\nH_0: B_Height = B_Weight = B_ApeIndex = 0 against H_a: not H_O:")
anova(update(model, ~ MaxPullUpReps + YearsClimbing), model)

# Part 11 ----------------------------------------------------------------------
message("\nH_0: B_ApeIndex = 0.5 against H_a: B_ApeIndex != 0.5:")
anova(update(model, ~ . - ApeIndex + offset(I(0.5*ApeIndex))), model)



# Part 12 ----------------------------------------------------------------------
message("\nH_0: B_Height = B_Weight against H_a:  B_Height != B_Weight:")
anova(update(model, ~ . -Height +offset(Weight)), model)
