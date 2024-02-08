########################
# Tutorial 4 solutions #
########################

## Load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

lapply(c("tidyverse"),  pkgTest)

## Reading in the data 
#  Option 1: 
#  Using stringsAsFactors
graduation <- read.table("http://statmath.wu.ac.at/courses/StatsWithR/Powers.txt",
                         stringsAsFactors = TRUE)
#  Option 2: 
#  Parse column names as a vector to colClasses
graduation <- read.table("http://statmath.wu.ac.at/courses/StatsWithR/Powers.txt",
                         colClasses = c("hsgrad" = "factor", 
                                        "nonwhite" = "factor",
                                        "mhs" = "factor",
                                        "fhs" = "factor",
                                        "intact" = "factor"))
summary(graduation)

# Drop problematic cases
graduation <- graduation[-which(graduation$nsibs < 0),]

#  Option 3: 
#  Coerce from a character vector to a logical vector
graduation$hsgrad <- as.logical(as.numeric(as.factor(graduation$hsgrad))-1) 

#  Option 4: 
#  Use ifelse() with as.logical()...
as.logical(ifelse(graduation$hsgrad == "Yes", 1, 0))

## a) Run the logit regression
mod <- glm(hsgrad ~ ., # period functions as omnibus selector (kitchen sink additive model)
           data = graduation, 
           family = "binomial")

mod <- glm(hsgrad ~ ., 
           data = graduation, 
           family = binomial(link = "logit")) # same as above (logit is default arg)

summary(mod)

## Likelihood ratio test
#  Create a null model
nullMod <- glm(hsgrad ~ 1, # 1 = fit an intercept only (i.e. sort of a "mean") 
               data = graduation, 
               family = "binomial")

#  Run an anova test on the model compared to the null model 
anova(nullMod, mod, test = "Chisq")
anova(nullMod, mod, test = "LRT") # LRT is equivalent

##  Extracting confidence intervals (of the coefficients)
?confint
exp(confint(mod)) # Remember: transform to odds ratio using exp()

# An option for making a data.frame of confidence intervals and coefficients
confMod <- data.frame(cbind(lower = exp(confint(mod)[,1]), 
                            coefs = exp(coef(mod)), 
                            upper = exp(confint(mod)[,2])))

# Then use this to make a plot
ggplot(data = confMod, mapping = aes(x = row.names(confMod), y = coefs)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), colour = "red") + 
  coord_flip() +
  labs(x = "Terms", y = "Coefficients")

# Looking at this plot, which terms are significant at the 0.05 level?

## b) Factor vs numeric
#  The nsibs variable was parsed as an integer. The model.matrix() function is 
#  used under the hood by lm() and glm() to create a design matrix of the model. 
#  See the difference compared to when we input nsibs as an integer and a 
#  factor:
?model.matrix

model.matrix( ~ unique(nsibs), data = graduation) # I see a problem with the data here...

# As a side note, we can use unique() with model.matrix() to create a matrix 
# of different combinations of factor levels to use with predict(). Though it's
# probably not the best approach...
model.matrix( ~ as.factor(unique(nsibs)), data = graduation)

# A better function to help with this is expand.grid()
with(graduation, expand.grid(nonwhite = unique(nonwhite),
                             mhs = unique(mhs),
                             fhs = unique(fhs)))

# Consider for instance if we had a model just consisting of factors:
mod2 <- glm(hsgrad ~ nonwhite + mhs + fhs, 
            data = graduation, 
            family = "binomial")

predicted_data <- with(graduation, expand.grid(nonwhite = unique(nonwhite),
                                               mhs = unique(mhs),
                                               fhs = unique(fhs)))
  
predicted_data <- cbind(predicted_data, predict(mod2, 
                                                newdata = predicted_data,
                                                type = "response",
                                                se = TRUE))

# Now we can use the code in Jeff's lecture to fill out the confidence intervals 
# and predicted probability (see lecture)
predicted_data <- within(predicted_data,
                         {PredictedProb <- plogis(fit)
                          LL <- plogis(fit - (1.96 * se.fit))
                          UL <- plogis(fit + (1.96 * se.fit))
                          })

# As an alternative to coercing an interval variable as a factor, with one 
# level for each unique value, we can "bin" the variable into a smaller number 
# of categories using cut()
?cut
graduation$nsibs_cut <- cut(graduation$nsibs, 
                            breaks = c(0, 0.9, 1, 3, Inf), 
                            include.lowest = TRUE,
                            labels = c("None", "One", "Two_Three", "FourPlus"))

mod3 <- glm(hsgrad ~., 
            data = graduation[,!names(graduation) %in% c("nsibs", "nsibs_f")], 
            family = "binomial")
summary(mod3)
summary(mod)

# Extract confidence intervals around the estimates
confMod3 <- data.frame(cbind(lower = exp(confint(mod3)[,1]), 
                             coefs = exp(coef(mod3)), 
                             upper = exp(confint(mod3)[,2])))

# Plot the estimates and confidence intervals
ggplot(data = confMod3, mapping = aes(x = row.names(confMod3), y = coefs)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), colour = "red") +
  coord_flip() +
  scale_y_continuous(breaks = seq(0,8,1)) +
  labs(x = "Terms", y = "Coefficients")
