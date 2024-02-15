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

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

## More on logits: visualising and goodness of fit

graduation <- read.table("http://statmath.wu.ac.at/courses/StatsWithR/Powers.txt",
                         stringsAsFactors = TRUE)

# 1. This time, let's analyse the data in more detail. Run some checks to see if 
#    the data are well distributed. Try some simple plots to get an idea of the 
#    relationship between variables. Drop those errors too.

# 2. Last week we created a kitchen sink model, with nsibs as a binned factor. 
#    Here was the code:
graduation$nsibs_cut <- cut(graduation$nsibs, 
                            breaks = c(0, 0.9, 1, 3, Inf), 
                            include.lowest = TRUE,
                            labels = c("None", "One", "Two_Three", "FourPlus"))

mod_1 <- glm(hsgrad ~., 
             data = graduation[,!names(graduation) %in% c("nsibs")], 
             family = "binomial")

# Create a more parsimonious model of your own choice. Select three predictor 
# variables, run the regression, and check with summary.

mod_2 <- #your model

# 3. a) Create a new data frame comprising the outcome variable and two columns 
#       of fitted values, one from mod_1 and another from mod_2. 

# 3. b) Create a pipe (without reassigning) whereby you reorder your new 
#       dataframe according to the fitted values of mod_1, create a new rank 
#       variable, then create a scatterplot of rank by fitted value, 
#       colored by the outcome variable.

# 3. c) Do the same for mod_2. Compare the results.

# 4. Calculate McFadden's Pseudo R squared for both models. 
#    Which model explains more variance?
#    What are the p values?

