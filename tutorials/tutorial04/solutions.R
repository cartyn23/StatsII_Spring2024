#######################
# Stats 2: tutorial 4 #
#######################

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

lapply(c("tidyverse", "car"),  pkgTest)

## More on logits: visualising and goodness of fit
# We'll use last weeks' data again
graduation <- read.table("http://statmath.wu.ac.at/courses/StatsWithR/Powers.txt",
                         stringsAsFactors = TRUE)

# This time, let's analyse the data in more detail
xtabs(~ hsgrad + nonwhite, data = graduation)
with(graduation, table(hsgrad, nonwhite))
summary(graduation)

# Find errors
graduation[which(graduation$nsibs < 0),]
graduation <- graduation[-which(graduation$nsibs < 0),]

# Model one: 
graduation$nsibs_cut <- cut(graduation$nsibs, 
                            breaks = c(0, 0.9, 1, 3, Inf), 
                            include.lowest = TRUE,
                            labels = c("None", "One", "Two_Three", "FourPlus"))

mod_1 <- glm(hsgrad ~., 
             data = graduation[,!names(graduation) %in% c("nsibs")], 
             family = "binomial")


# A more parsimonious model
mod_2 <- glm(hsgrad ~ nsibs_cut + income + nonwhite, 
             data = graduation, 
             family = "binomial")

summary (mod_2)

# Make a data frame
predicted_data <- data.frame(
  hsgrad = graduation$hsgrad,
  mod_1_hat = mod_1$fitted.values,
  mod_2_hat = mod_2$fitted.values
)

# Reorder and plot
predicted_data %>%
  arrange(mod_1_hat) %>%
  mutate(rank = row_number()) %>%
  ggplot(aes(rank, mod_1_hat)) +
  geom_point(aes(colour = hsgrad), alpha = 0.5) +
  scale_y_continuous(limits = c(0,1))

predicted_data %>%
  arrange(mod_2_hat) %>%
  mutate(rank = row_number()) %>%
  ggplot(aes(rank, mod_2_hat)) +
  geom_point(aes(colour = hsgrad), alpha = 0.5) +
  scale_y_continuous(limits = c(0,1))

# McFadden's R squared
# Approach 1:
mod_1$null.deviance == mod_2$null.deviance

ll.null <- mod_1$null.deviance/-2
ll.fit_1 <- mod_1$deviance/-2
ll.fit_2 <- mod_2$deviance/-2

r.sq.mod_1 <- (ll.null-ll.fit_1)/ll.null
r.sq.mod_2 <- (ll.null-ll.fit_2)/ll.null

# Approach 2:
mod_null <- glm(hsgrad ~ 1, data = graduation, family = "binomial")
1 - logLik(mod_1)/logLik(mod_null)
1 - logLik(mod_2)/logLik(mod_null)

# P value:
1 - pchisq(2*(ll.fit_1 - ll.null), df = (length(mod_1$coefficients)-1))
1 - pchisq(2*(ll.fit_2 - ll.null), df = (length(mod_2$coefficients)-1))
