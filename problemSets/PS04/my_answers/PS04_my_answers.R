#########################
#PS04 SURVIVAL ANALYSIS
########################

# Question: We’re interested in modeling the historical causes of child mortality. We have data from
# 26855 children born in Skellefte˚a, Sweden from 1850 to 1884. Using the ”child” dataset in
# the eha library, fit a Cox Proportional Hazard model using mother’s age and infant’s gender
# as covariates. Present and interpret the output.

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

lapply(c("survival", "eha", "tidyverse", "ggfortify", "stargazer"),  pkgTest)

# Using the ”child” dataset 

data(child)

child_surv <- with(child, Surv(enter, exit, event))

# fit a Cox Proportional Hazard model using mother’s age and infant’s gender
cox <- coxph(child_surv ~ sex + m.age, data = child)
summary(cox)

# check fit
drop1(cox, test = "Chisq")
stargazer(cox, type = "text")

# There is a 0.082 decrease in the expected log of the hazard for female babies compared to 
# male, holding mother's age constant. There is a 0.008 increase in the expected log of the hazard
# for babies of mothers as they age by a year, holding sex constant.


# exponentiate parameter estimates to obtain hazard ratios
exp(-0.082)
exp(0.008)

# The hazard ratio of female babies is 0.92 that of male babies, i.e. female babies are less
# likely to die (92 female babies die for every 100 male babies; female deaths are 8% lower, etc.)

# Present the results ...
cox_fit <- survfit(cox)
autoplot(cox_fit)

# a further plot

newdat <- with(child, 
               data.frame(
               sex = c("female", "male"), m.age = rep(mean(m.age,na.rm = TRUE, 2))
)
)

# using an average age
fit <- survfit(cox, newdata = newdat)

plot(survfit(cox, newdata = newdat), xscale = 1,
     conf.int = T,
     ylim = c(0.6, 1),
     col = c("red", "blue"),
     xlab = "Time",
     ylab = "Survival proportion",
     main = "")
legend("bottomleft",
       legend=c("Male", "Female"),
       lty = 1, 
       col = c("red", "blue"),
       text.col = c("red", "blue"))

# Plotting cumulative hazard function
plot_COXPH <- coxreg(child_surv ~ sex + m.age, data = child)
plot(plot_COXPH)

# a new plot

newdat20 <- with(child, expand.grid(m.age = c(20, 40), sex = c("male", "female")))


plot(survfit(cox, newdata = newdat), xscale = 0.30,
     conf.int = T,
     ylim = c(0.75, 1),
     col = c("red", "blue"),
     xlab = "Time",
     ylab = "Survival proportion",
     main = "")
legend("bottomleft",
       legend=c("Male", "Female"),
       lty = 1, 
       col = c("red", "blue"),
       text.col = c("red", "blue"))

plot(survfit(cox, newdata = newdat), 
     xscale = 0.30,
     conf.int = TRUE,
     ylim = c(0.75, 1),
     col = c("red", "blue", "green", "black"), # Specify colors for each age group and sex
     xlab = "Time",
     ylab = "Survival proportion",
     main = "")
legend("topright",
       legend=c("Male (20)", "Male (40)", "Female (20)", "Female (40)"), 
       lty = 1, 
       col = c("red", "blue", "green", "black"),
       text.col = c("red", "blue", "green", "black"),
       cex = 0.6)

