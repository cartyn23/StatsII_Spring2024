#######################
# Tutorial 9: Poisson #
#######################

library(tidyverse)
library(ggplot2)

long_data <- read.table("http://statmath.wu.ac.at/courses/StatsWithR/Long.txt", header=T)

# wrangle
long_data <- within(long_data, {
  fem <- as.logical(fem)
  mar <- as.logical(mar)
})

# EDA
str(long_data)
summary(long_data)

with(long_data,
     list(mean(art), var(art))) # do we meet assumptions for Poisson?
     
# a) Examine distribution of response variable
hist(long_data$art)

ggplot(long_data, aes(ment, art, color = fem)) +
  geom_jitter(alpha = 0.5)

# OLS?
mod.lm <- lm(art ~ ., data = long_data)
summary(mod.lm)

mod2.lm <- lm(art ~ fem * ., data = long_data) # interaction effects with gender?
summary(mod2.lm)

# Do we meet assumptions?
plot(predict(mod2.lm), abs(resid(mod2.lm)), xlab = "Predicted", ylab = "Absolute Residuals")

sresid <- rstandard(mod2.lm) # distribution of standardised residuals
hist(sresid, main = "")

par(mfrow = c(2, 2))
plot(mod2.lm) # R's built-in plots for linear regression model assessment

# b) Poisson regression
mod.ps <- glm(art ~ ., data = long_data, family = poisson)
summary(mod.ps)

# interpreting outputs
cfs <- coef(mod.ps)

# predicted no. of articles for a married male PhD researcher with 1 child at 2-rated 
# institute whose PhD supervisor published 5 articles.
exp(cfs[1] + cfs[2]*0 + cfs[3]*5 + cfs[4]*2 + cfs[5]*1 + cfs[6]*1)

pred <- data.frame(fem = FALSE,
                   ment = 5,
                   phd = 2,
                   mar = TRUE,
                   kid5 = 1)

# check with predict() function
predict(mod.ps, newdata = pred, type = "response")

# plot predictions vs count
ggplot(data = NULL, aes(x = mod.ps$fitted.values, y = long_data$art)) +
  geom_jitter(alpha = 0.5) +
  geom_abline(color = "blue") #+
  #geom_smooth(method = "loess", color = "red")

# calculate pseudo R squared
1 - (mod.ps$deviance/mod.ps$null.deviance)

# calculate RMSE
sqrt(mean((mod.ps$model$art - mod.ps$fitted.values)^2))

# Add an interaction?
mod2.ps <- glm(art ~ fem * ., data = long_data, family = poisson)
summary(mod2.ps)


1 - (mod2.ps$deviance/mod2.ps$null.deviance) # pseudo R2
sqrt(mean((mod2.ps$model$art - mod2.ps$fitted.values)^2)) # RMSE

# c) Over-dispersion?
install.packages("AER")
library(AER)

dispersiontest(mod.ps)

install.packages("pscl")
library(pscl)

mod.zip <- zeroinfl(art ~ ., data = long_data, dist = "poisson")
summary(mod.zip)
