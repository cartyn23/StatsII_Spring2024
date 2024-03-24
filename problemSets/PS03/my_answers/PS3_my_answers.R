#####################
# load libraries
# set wd
# clear global .envir
#####################

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

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("nnet", "MASS"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Problem 1
#####################
# UNORDERED

# load data
gdp_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/gdpChange.csv", stringsAsFactors = F)

summary(gdp_data)

# UNORDERED ANALYSIS

gdp_data$GDPWdiff_New <- factor(ifelse(gdp_data$GDPWdiff > 0, "positive", ifelse(gdp_data$GDPWdiff < 0, "negative", "no change")),
                                levels = c("positive", "no change", "negative"),
                                labels = c("positive", "no change", "negative"))

gdp_data$REG <- factor(gdp_data$REG, levels = c(1, 0),
                       labels = c("Democracy", "Non-Democracy"))


gdp_data$OIL <- factor(gdp_data$OIL, levels = c(1, 0), labels = c("Exceed 50%", "Otherwise"))

# Fit the multinomial logistic regression model on the  data

gdp_data$REG <- relevel(gdp_data$REG, ref = "Non-Democracy")
gdp_data$OIL <- relevel(gdp_data$OIL, ref = "Otherwise")
gdp_data$GDPWdiff_New <- relevel(gdp_data$GDPWdiff_New , ref = "no change")

multinom_model1 <- multinom(GDPWdiff_New ~ REG + OIL + COUNTRY, data = gdp_data)
summary(multinom_model1)
exp(coef(multinom_model1))

# get p values
z <- summary(multinom_model1)$coefficients/summary(multinom_model1)$standard.errors
(p <- (1 - pnorm(abs(z), 0, 1)) * 2)

# how do we interpret these coefficients?

# we can use predicted probabilities to help our interpretation
pp <- data.frame(fitted(multinom_model1))
head(data.frame(GDP_Change = gdp_data$GDPWdiff_New))


#Construct and interpret an ordered multinomial logit with GDPWdiff as the outcome
# variable, including the estimated cutoff points and coefficients.

# ORDERED - PROPORTIONAL ODDS

ordered_model <- polr(GDPWdiff_New ~ REG + OIL + COUNTRY, data = gdp_data, Hess=TRUE)

# Print the summary of the model
summary(ordered_model)


#####################
# Problem 2
#####################

# load data
mexico_elections <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/MexicoMuniData.csv")

# Poisson regression
mod.ps <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06, data = mexico_elections, family = poisson)
summary(mod.ps)

# interpreting outputs
cfs <- coef(mod.ps)
cfs
