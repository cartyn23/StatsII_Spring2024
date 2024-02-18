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

lapply(c("stringr", "tidyverse"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# PART 1

# Fit an additive model. Provide the summary output, the global null hypothesis,
# and p-value. Please describe the results and provide a conclusion.

# load data
load(url("https://github.com/ASDS-TCD/StatsII_Spring2024/blob/main/datasets/climateSupport.RData?raw=true"))

data <- climateSupport
head(data)
summary(data)
str(data)

data$choice <- as.factor(ifelse(data$choice == "Supported", 1, 0))
data$choice

data$countries <- factor(data$countries, ordered = FALSE, levels = c("20 of 192", "80 of 192", "160 of 192"), labels = c("_20_192", "_80_192", "_160_192"))
data$sanctions <- factor(data$sanctions, ordered = FALSE, levels = c("None", "5%", "15%", "20%"), labels = c("none", "_5_percent", "_15_percent", "_20_percent"))

data$countries <- as.factor(data$countries)
data$sanctions <- as.factor(data$sanctions)

logit_base <- glm(choice ~ sanctions + countries, data = data, family = binomial(link = "logit"))
summary(logit_base)

nullMod <- glm(choice ~ 1, # 1 = fit an intercept only (i.e. sort of a "mean") 
               data = data, 
               family = "binomial")

summary(nullMod)


#log-odds

exp(0.0637)/(1+exp(0.0637))


#  Run an anova test on the model compared to the null model 
anova(nullMod, logit_base, test = "Chisq")


# Add an interactive term

Model_1 <- glm(choice ~ sanctions + countries, data = data, family = binomial(link = "logit"))
Model_Interactive <- glm(choice ~ sanctions * countries, data = data, family = binomial(link = "logit"))

summary(Model_Interactive)

anova(logit_base, Model_Interactive, test = "LRT")
,