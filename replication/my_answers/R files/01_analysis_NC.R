###############################
### Replication Materials for
### Stefan Müller and Aidan Regan:
### Are Irish Voters Moving to the Left?
### Irish Political Studies
###
### Please get in touch with the authors if you have any questions: 
### stefan.mueller@ucd.ie

### 00a_filter_and_harmonise_lr_surveys.R
### Reproduce all tables and graphs from the 
### main paper and appendix
###############################


# load packages
library(Hmisc)
library(here) 
library(stringr)
library(srvyr)
library(forcats)
library(texreg)
library(nnet)
library(forcats)
library(scales)
library(effects)
library(tidyr)
library(randomForest)
library(dplyr)
library(ggplot2)
library(ggeffects)


# set working directory

setwd("C:/Users/carty/Downloads/OneDrive/Desktop/REPLICATION")

# alternatively, set working directory using setwd()

# load custom ggplot2 scheme
theme_baser <- function (){
    theme_minimal()  %+replace%
        theme(panel.grid.minor.x = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.border = element_rect(fill=NA,color="black", size=0.5,
                                          linetype="solid"),
              #panel.border = element_rect(color = "black", fill = NA, size = 1),
              legend.title = element_text(size = 15),
              title = element_text(size = 15, vjust = 1.5, hjust = 0),
              legend.position = "bottom",
              axis.ticks = element_line(size = 0.3),
              axis.ticks.length = unit(0.2,"cm"),
              legend.text=element_text(size = 13),
              strip.text = element_text(size = 13, hjust = 0.5, face = "bold",
                                        margin=ggplot2::margin(b=5, r = 5, l = 5, t = 5)),
              axis.text = element_text(colour="black", size = 13),
              axis.title = element_text(size = 13, hjust = 0.5))
}

theme_set(theme_baser())


# load harmonised survey dataset (created in 00a_filter_and_harmonise_lr_surveys.R)

data_surveys_1973_2020 <- readRDS("data_surveys_1973_2020.rds")


# load dataset with harmonised election studies

dat_electionstudies <- readRDS("data_election_studies_ireland.rds")

table(dat_electionstudies$left_right_self,
      dat_electionstudies$year)

# descriptive statistics of surveys with Irish respondents, 1973-2020

# filter only Irish respondents
dat_ire <- filter(data_surveys_1973_2020, 
                  country == "Ireland")


# number of years
length(unique(dat_ire$year))
# 47 years

# number of valid responses from Irish citizens
dat_ire %>% 
    filter(!is.na(left_right0to10)) %>% 
    filter(!is.na(year)) %>% 
    nrow()
# 152344 valid responses


# Sources: CSES, ESS, Eurobarometer
table(dat_ire$dataset)


set.seed(14)
dat_ire_sum <- dat_ire %>%
    group_by(year) %>%
    do(data.frame(rbind(Hmisc::smean.cl.boot(.$left_right0to10))))


## Figure 1 ----
ggplot(dat_ire_sum, aes(x = year, y = Mean, 
                        ymin = Lower, ymax = Upper)) +
    geom_smooth(fill = "grey80", colour = "black", alpha = 1) + 
    geom_point(size = 3, fill = "grey40", colour = "grey40") +
    geom_linerange(colour = "grey40") +
    scale_x_continuous(breaks = c(seq(1975, 2020, 5))) +
    labs(x = "Survey Year", y = "Average Left-Right Self-Placement")
ggsave("fig_01.pdf",
       width = 9, height = 5)


## Figure 2 ----

table(dat_electionstudies$party_vote_recoded_precise,
      dat_electionstudies$year)

# select only a subset of parties for Figure 2
dat_all_elections_subset <- dat_electionstudies %>% 
    filter(party_vote_recoded_precise %in% c(
        "Solidarity PBP",
        "Social Democrats", "Sinn Féin",
        "Green Party", "Labour Party",
        "Fianna Fáil", "Fine Gael"
    ))


left_right_self_partymeans <- dat_all_elections_subset %>%
    srvyr::as_survey_design() %>% 
    group_by(year, party_vote_recoded_precise) %>%
    summarise(lr_mean = srvyr::survey_mean(left_right_self, 
                                           na.rm = TRUE)) %>% 
    mutate(lr_ci_95_lower = lr_mean - 1.96 * lr_mean_se) %>% 
    mutate(lr_ci_95_upper = lr_mean + 1.96 * lr_mean_se) %>% 
    mutate(lr_ci_90_lower = lr_mean - 1.645 * lr_mean_se) %>% 
    mutate(lr_ci_90_upper = lr_mean + 1.645 * lr_mean_se) 

# reorder parties
left_right_self_partymeans$party_vote_recoded_precise <- factor(
    left_right_self_partymeans$party_vote_recoded_precise,
    levels = c("Fine Gael", 
               "Fianna Fáil",
               "Labour Party",
               "Green Party", 
               "Sinn Féin",
               "Social Democrats", 
               "Solidarity PBP"))

# assign colours
# https://en.wikipedia.org/wiki/Category:Ireland_political_party_colour_templates
colours_party <- c("#009FF3",
                   "#66BB66",
                   "#CC0000",
                   "#99CC33",
                   "#326760",
                   "#752F8B",
                   "#660000")



ggplot(left_right_self_partymeans, 
       aes(x = forcats::fct_rev(as.factor(year)), 
           y = lr_mean,
           colour = party_vote_recoded_precise)) +
    geom_point(size = 2) +
    # geom_linerange(aes(ymin = lr_ci_90_lower,
    #                    ymax = lr_ci_90_upper),
    #                size = 1.05) +
    geom_linerange(aes(ymin = lr_ci_95_lower,
                       ymax = lr_ci_95_upper)) +
    coord_flip() +
    scale_y_continuous(limits = c(0, 10), 
                       breaks = c(seq(0, 10, 1))) +
    scale_colour_manual(values = colours_party) +
    facet_grid(party_vote_recoded_precise ~.) +
    theme(legend.position = "none",
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 12),
          strip.text.y = element_text(angle = 0, hjust = 0, size = 12))  +
    labs(x = "Election",  
         y = "Average Left-Right Self-Placement")
ggsave("fig_02.pdf",
       width = 9, height = 7.5)


## Linear regression models ----


dat_reg <- dat_electionstudies

summary(dat_reg)

# adjust factor variables
dat_reg$income_harmonised <- as.factor(dat_reg$income_harmonised)


dat_reg$gender <- relevel(as.factor(dat_reg$gender), 
                          ref = "Male")
dat_reg$urban <- relevel(as.factor(dat_reg$urban), 
                         ref= "0")

dat_reg$university_degree <- relevel(as.factor(dat_reg$university_degree), 
                                     ref = "0")

dat_reg$income_harmonised <- relevel(as.factor(dat_reg$income_harmonised), 
                                     ref = "1")

dat_reg$party_vote <- relevel(as.factor(dat_reg$party_vote), 
                              ref = "Fianna Fáil")

dat_reg$age_cat <- factor(dat_reg$age_cat)


# models with left-right self-placements as DV

# 2002
lm_lr_02 <- lm(left_right_self ~ 
                   income_harmonised  +
                   age_cat +
                   gender + urban +
                   university_degree,
               weight = weights,
               data = filter(dat_reg,
                             year == "2002"))

# 2007
lm_lr_07 <- update(lm_lr_02,  
                   data = filter(dat_reg,
                                 year == "2007"))

# 2011
lm_lr_11 <- update(lm_lr_02,  
                   data = filter(dat_reg,
                                 year == "2011"))

# 2011
lm_lr_16 <- update(lm_lr_02,  
                   data = filter(dat_reg,
                                 year == "2016"))


# 2020
lm_lr_20 <- update(lm_lr_02,  
                   data = filter(dat_reg,
                                 year == "2020"))



## Table 1 ----
screenreg(list(
    lm_lr_02,
    lm_lr_07,
    lm_lr_11,
    lm_lr_16,
    lm_lr_20
))


wordreg(list(lm_lr_02,
             lm_lr_07,
             lm_lr_11,
             lm_lr_16,
             lm_lr_20),
        single.row = FALSE,
        custom.coef.names = c(
            "(Intercept)",
            "Income category: 2 (ref.: 1)",
            "Income category: 3",
            "Income category: 4",
            "Income category: 5",
            "Age: 25-34 (ref.: 18-24)",
            "Age: 35-44",
            "Age: 45-54",
            "Age: 55-64",
            "Age: 65+",
            "Female",
            "Urban constituency",
            "University degree"),
        size = "footnotesize",
        custom.model.names = c("2002", "2007", "2011", "2016", "2020"),
        file = "tab_01.doc")


## Figure 3 ----


## get expected values of income levels for each election


# 2002
pred_income_02 <- ggpredict(lm_lr_02, terms = c("income_harmonised"),
                            condition = c(
                                age_cat = "35-44",
                                gender = "Male",
                                urban = "0",
                                university_degree = "0")) %>% mutate(model = "2002")
pred_income_02


# 2007
pred_income_07 <- ggpredict(lm_lr_07, terms = c("income_harmonised"),
                            condition = c(
                                age_cat = "35-44",
                                gender = "Male",
                                urban = "0",
                                university_degree = "0")) %>% mutate(model = "2007")

pred_income_07

# 2011
pred_income_11 <- ggpredict(lm_lr_11, terms = c("income_harmonised"),
                            condition = c(
                                age_cat = "35-44",
                                gender = "Male",
                                urban = "0",
                                university_degree = "0")) %>% mutate(model = "2011")
pred_income_11


# 2016
pred_income_16 <- ggpredict(lm_lr_16, terms = c("income_harmonised"),
                            condition = c(
                                age_cat = "35-44",
                                gender = "Male",
                                urban = "0",
                                university_degree = "0")) %>% mutate(model = "2016")
pred_income_16

# 2020
pred_income_20 <- ggpredict(lm_lr_20, terms = c("income_harmonised"),
                            condition = c(
                                age_cat = "35-44",
                                gender = "Male",
                                urban = "0",
                                university_degree = "0")) %>% mutate(model = "2020")
pred_income_20

# bind expected values from all models
pred_income <- bind_rows(pred_income_02,
                         pred_income_07,
                         pred_income_11,
                         pred_income_16,
                         pred_income_20)

pred_income <- pred_income %>% 
  filter(!is.na(x))

# Change labels of income categories
#pred_income <- pred_income %>% 
  #mutate(income_cat = dplyr::recode(
   # x, "1" = "1: Lowest", "5" = "5: Highest"
 # ))

ggplot(pred_income, aes(x = predicted, y = x)) +
    geom_point(size = 3) +
    geom_errorbarh(aes(xmin = predicted - 1.96 * std.error,
                       xmax = predicted + 1.96 * std.error),
                   size = 0.5, height = 0) +
    # geom_errorbarh(aes(xmin = predicted - 1.645 * std.error,
    #                    xmax = predicted + 1.645 * std.error),
    #                size = 1.3, height = 0) +
    coord_flip() +
    facet_wrap(~model, nrow = 1) +
    scale_x_continuous(limits = c(3, 8)) +
    labs(x = "Expected Left-Right Self-Placement",
         y = "Income Category (from Lowest to Highest)")
ggsave("fig_03.pdf", 
       width = 9, height = 5)


## Multinomial regression models ----

## predict party choice conditional on left-right self-placement

# get years
years <- unique(dat_reg$year)

# empty dataframe to store predicted probabilities
dat_multinom_merged <- data.frame()

for (i in years) {
    
    dat_year <- filter(dat_reg, year == i)
    
    lm_multinom <- multinom(party_vote ~ left_right_self + income_harmonised + age_cat  + 
                                gender + 
                                urban + university_degree,
                            weight = weights,
                            data = dat_year)
    
    
    aic_election <- lm_multinom$AIC
    dat_effect_lr <- as.data.frame(
        Effect(c("left_right_self"), 
               lm_multinom, xlevels = 20))
    
    dat_effect_lr_prob <- dat_effect_lr %>% 
        select(c(left_right_self, starts_with("prob.")))
    
    dat_effect_lr_prob_long <- dat_effect_lr_prob %>% 
        gather(party_vote_aggregated, predicted, -c(left_right_self)) %>% 
        mutate(party_vote_aggregated = str_replace_all(party_vote_aggregated, "prob.", ""))
    
    
    dat_effect_lr_se <- dat_effect_lr %>% 
        select(c(left_right_self, starts_with("se.prob.")))
    
    dat_effect_lr_se_long <- dat_effect_lr_se %>% 
        gather(party_vote_aggregated, std.error, -c(left_right_self)) %>% 
        mutate(party_vote_aggregated = str_replace_all(party_vote_aggregated, "se.prob.", ""))
    
    dat_effect_lr_se_long <- left_join(dat_effect_lr_prob_long,
                                       dat_effect_lr_se_long,
                                       by = c("party_vote_aggregated", "left_right_self")) %>% 
        mutate(party_vote_aggregated = str_replace_all(party_vote_aggregated, "\\.", " ")) 
    
    dat_effect_lr_se_long$year <- i
    dat_effect_lr_se_long$aic_election <- aic_election
    
    dat_multinom_merged <- bind_rows(dat_effect_lr_se_long,
                                     dat_multinom_merged)
    
}

# colours for parties and levels for factors
colours_party <- c("#66BB66", "#009FF3", "#326760",
                   "red",
                   "grey70")

factors_party <- c("Fianna Fáil", "Fine Gael",
                   "Sinn Féin",
                   "Greens and Left bloc",
                   "Other and Independents")



dat_multinom_merged$party_vote_aggregated <- factor(dat_multinom_merged$party_vote_aggregated,
                                                    levels = factors_party)

# determine confidence intervals
ci_90 <- 1.645
ci_95 <- 1.96

## Figure 4 ----
ggplot(dat_multinom_merged, aes(x = left_right_self, 
                                y = predicted)) +
    # geom_ribbon(aes(ymin = predicted - ci_90 * std.error,
    #                 ymax = predicted + ci_90 * std.error,
    #                 fill = party_vote_aggregated)) +
    geom_ribbon(aes(ymin = predicted - ci_95 * std.error,
                    ymax = predicted + ci_95 * std.error,
                    fill = party_vote_aggregated)) +
    geom_line() +
    scale_fill_manual(values = colours_party) +
    scale_x_continuous(breaks = c(seq(0, 10, 2))) +
    scale_colour_grey(name = "Income", start = 0.7, end = 0) +
    facet_grid(party_vote_aggregated~year, scales = "free_x", 
               labeller = label_wrap_gen(width = 15)) +
    labs(x = "Left-right self-placement", y = "Pr(Party as First-Preference Vote Choice)") +
    theme(legend.position = "none", 
          legend.title = element_blank())
ggsave("fig_04.pdf", 
       width = 9, height = 10)


## Repeat multinomial logistic regression models for 2020 with different set of independent variables

dat_reg_multinom <- dat_reg

head(dat_reg_multinom)

multinom_20_incomediff <- multinom(party_vote ~ income_differences + income_harmonised + 
                                       age_cat  + 
                                       gender + 
                                       urban + university_degree,
                                   weight = weights,
                                   data = filter(dat_reg_multinom, 
                                                 year == "2020"))

multinom_20_incomediff

multinom_20_taxespend <- multinom(party_vote ~ taxes_spending + 
                                      income_harmonised +
                                      age_cat  + 
                                      gender +  
                                      urban + university_degree,
                                  weight = weights,
                                  data = filter(dat_reg_multinom, 
                                                year == "2020"))


# get predicted probabilities for income differences
dat_effect_incomediff_2020 <- as.data.frame(
    Effect(c("income_differences"), 
           multinom_20_incomediff, xlevels = 20))

dat_effect_incomediff_2020_prob <- dat_effect_incomediff_2020 %>% 
    select(c(income_differences, starts_with("prob.")))

dat_effect_incomediff_prob_2020_long <- dat_effect_incomediff_2020_prob %>% 
    gather(party_vote_aggregated, predicted, -c(income_differences)) %>% 
    mutate(party_vote_aggregated = str_replace_all(party_vote_aggregated, "prob.", ""))

dat_effect_incomediff_2020_se <- dat_effect_incomediff_2020 %>% 
    select(c(income_differences, starts_with("se.prob.")))

dat_effect_incomediff_se_2020_long <- dat_effect_incomediff_2020_se %>% 
    gather(party_vote_aggregated, std.error, -c(income_differences)) %>% 
    mutate(party_vote_aggregated = str_replace_all(party_vote_aggregated, "se.prob.", ""))

dat_effect_incomediff_2020_se_long <- left_join(dat_effect_incomediff_prob_2020_long,
                                                dat_effect_incomediff_se_2020_long,
                                                by = c("party_vote_aggregated", "income_differences")) %>% 
    mutate(party_vote_aggregated = str_replace_all(party_vote_aggregated, "\\.", " ")) 




dat_effect_incomediff_2020_se_long$party_vote_aggregated <- 
    factor(dat_effect_incomediff_2020_se_long$party_vote_aggregated,
           levels = factors_party)

# colours for parties and levels for factors
colours_party <- c("#66BB66", "#009FF3", "#326760",
                   "red",
                   "grey70")


## Figure 5 ----
ggplot(dat_effect_incomediff_2020_se_long, aes(x = income_differences, 
                                               y = predicted)) +
    # geom_ribbon(aes(ymin = predicted - ci_90 * std.error,
    #                 ymax = predicted + ci_90 * std.error,
    #                 fill = party_vote_aggregated)) +
    geom_ribbon(aes(ymin = predicted - ci_95 * std.error,
                    ymax = predicted + ci_95 * std.error,
                    fill = party_vote_aggregated)) +
    geom_line() +
    scale_fill_manual(values = colours_party) +
    scale_x_continuous(breaks = c(seq(0, 10, 2))) +
    scale_colour_grey(name = "Income", start = 0.7, end = 0) +
    facet_wrap(~party_vote_aggregated,
               labeller = label_wrap_gen(width = 15),
               nrow = 1) +
    scale_y_continuous(limits = c(0, 0.6),
                       breaks = c(seq(0, 0.6, 0.1))) +  
    labs(x = "Reduce Differences in Income and Wealth", y = "Pr(Party as First-Preference Vote Choice)") +
    theme(legend.position = "none", 
          legend.title = element_blank())
ggsave("fig_05.pdf",
       width = 9, height = 4.5)


# get predicted values for taxes and spending
dat_effect_tax_2020 <- as.data.frame(
    Effect(c("taxes_spending"), 
           multinom_20_taxespend, xlevels = 20))

dat_effect_tax_2020_prob <- dat_effect_tax_2020 %>% 
    select(c(taxes_spending, starts_with("prob.")))

dat_effect_tax_prob_2020_long <- dat_effect_tax_2020_prob %>% 
    gather(party_vote_aggregated, predicted, -c(taxes_spending)) %>% 
    mutate(party_vote_aggregated = str_replace_all(party_vote_aggregated, "prob.", ""))

dat_effect_tax_2020_se <- dat_effect_tax_2020 %>% 
    select(c(taxes_spending, starts_with("se.prob.")))

dat_effect_tax_se_2020_long <- dat_effect_tax_2020_se %>% 
    gather(party_vote_aggregated, std.error, -c(taxes_spending)) %>% 
    mutate(party_vote_aggregated = str_replace_all(party_vote_aggregated, "se.prob.", ""))

dat_effect_tax_2020_se_long <- left_join(dat_effect_tax_prob_2020_long,
                                         dat_effect_tax_se_2020_long,
                                         by = c("party_vote_aggregated", "taxes_spending")) %>% 
    mutate(party_vote_aggregated = str_replace_all(party_vote_aggregated, "\\.", " ")) 



dat_effect_tax_2020_se_long$party_vote_aggregated <- 
    factor(dat_effect_tax_2020_se_long$party_vote_aggregated,
           levels = factors_party)

library(shades)

## Figure 6 ----
ggplot(dat_effect_tax_2020_se_long, aes(x = taxes_spending, 
                                        y = predicted)) +
    # geom_ribbon(aes(ymin = predicted - ci_90 * std.error,
    #                 ymax = predicted + ci_90 * std.error,
    #                 fill = party_vote_aggregated)) +
    geom_ribbon(aes(ymin = predicted - ci_95 * std.error,
                    ymax = predicted + ci_95 * std.error,
                    fill = party_vote_aggregated)) +
    geom_line() +
    scale_fill_manual(values = colours_party) +
    scale_x_continuous(breaks = c(seq(0, 10, 2))) +
    scale_colour_grey(name = "Income", start = 0.7, end = 0) +
    facet_wrap(~party_vote_aggregated,
               labeller = label_wrap_gen(width = 15),
               nrow = 1) +
    scale_y_continuous(limits = c(0, 0.6),
                       breaks = c(seq(0, 0.6, 0.1))) +
    labs(x = "More Taxes and Spending", y = "Pr(Party as First-Preference Vote Choice)") +
    theme(legend.position = "none", 
          legend.title = element_blank())
ggsave("fig_06.pdf",
       width = 9, height = 4.5)



# Plots and Tables for Online Appendix ----


## Most important issues ----

## Load the Exit Poll to determine most important issues in 2020

# Exit poll can be downloaded at INES 2020 Dataverse
# but is also included in the replication materials
#  https://doi.org/10.7910/DVN/HJIB3S, Harvard Dataverse, V1, 


load("2020 Exit Poll.RData")

# recode most important issue
dat <- x %>% 
    mutate(mostImportant = dplyr::recode(mostImportant, 
                                         "Something else" = "Other")) %>% 
    mutate(firstPrefRec = dplyr::recode(firstPrefRec,
                                        "FF" = "Fianna Fáil",
                                        "FG" = "Fine Gael",
                                        "Greens" = "Green Party",
                                        "Indep" = "Independents/Other",
                                        "Other" = "Independents/Other",
                                        "SF" = "Sinn Féin",
                                        "SocDems" = "Social Democrats",
                                        "Sol-PBP" = "Solidarity-PBP"
    )) 

table(dat$mostImportant)


# get most important issue and CIs

dat_mip <- dat %>% 
    filter(!is.na(mostImportant)) %>% 
    group_by(mostImportant) %>% 
    count() %>% 
    ungroup() %>% 
    mutate(n_valid = sum(n),
           prop = n / n_valid,
           ci_lower90 =  prop - 1.645 * sqrt((prop * (1 - prop))/ n_valid),
           ci_upper90 =  prop + 1.645 * sqrt((prop * (1 - prop))/ n_valid),
           ci_lower95 =  prop - 1.96 * sqrt((prop * (1 - prop))/ n_valid),
           ci_upper95 =  prop + 1.96 * sqrt((prop * (1 - prop))/ n_valid)) %>% 
    mutate(mostImportant = str_to_title(mostImportant))

dat_mip 

## Figure A01 ----
ggplot(dat_mip, aes(x = reorder(mostImportant, prop),
                    y = prop)) +
    geom_point(size = 3) +
    geom_linerange(aes(ymin = ci_lower95,
                       ymax = ci_upper95)) +
    geom_linerange(aes(ymin = ci_lower90,
                       ymax = ci_upper90),
                   size = 1.05) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    coord_flip() +
    labs(x = NULL, 
         y = "Mentioned as Most Important Issue") 
ggsave("fig_a01.pdf",
       width = 9, height = 4.5)



# check vote choice based on whether housing/homelessness was mentioned
# as the most important issue
dat_mip_party <- dat %>% 
    filter(!is.na(mostImportant)) %>% 
    mutate(mostImportant = str_to_title(mostImportant)) %>% 
    mutate(housing_mip = ifelse(mostImportant == "Housing/Homelessness",
                                "Housing/Homelessness", "Other Issue")) %>% 
    group_by(housing_mip, firstPrefRec) %>% 
    mutate(housing_mip = paste0("MIP: ", housing_mip)) %>% 
    count() %>% 
    group_by(housing_mip) %>% 
    mutate(n_valid = sum(n),
           prop = n / n_valid,
           ci_lower90 =  prop - 1.645 * sqrt((prop * (1 - prop))/ n_valid),
           ci_upper90 =  prop + 1.645 * sqrt((prop * (1 - prop))/ n_valid),
           ci_lower95 =  prop - 1.96 * sqrt((prop * (1 - prop))/ n_valid),
           ci_upper95 =  prop + 1.96 * sqrt((prop * (1 - prop))/ n_valid))



factors_party_mip <- c("Sinn Féin",
                   "Fianna Fáil",
                   "Fine Gael",
                   "Green Party",
                   "Labour",
                   "Social Democrats",
                   "Solidarity-PBP",
                   "Independents/Other")


# https://en.wikipedia.org/wiki/Category:Ireland_political_party_colour_templates
colours_party_mip <- c("grey70",
                   "#8E2420",
                   "#752F8B",
                   "#CC0000",
                   "#99CC33",
                   "#009FF3",
                   "#66BB66",
                   "#326760")


dat_mip_party$firstPrefRec <- factor(dat_mip_party$firstPrefRec,
                                     levels = factors_party_mip)


## Figure A02 ----
ggplot(data = dat_mip_party, 
       aes(x = forcats::fct_rev(firstPrefRec),
           y = prop,
           fill = firstPrefRec)) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = ci_lower95,
                      ymax = ci_upper95),
                  width = 0.3,
                  colour = "grey50") +
    facet_wrap(~housing_mip) +
    scale_fill_manual(values = rev(colours_party_mip)) +
    coord_flip() + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       limits = c(0, 0.4),
                       breaks = c(seq(0, 0.4, 0.1))) +
    labs(y = "Percent (First-Preference Vote Choice)", x = NULL) +
    theme(legend.position = "none")
ggsave("fig_a02.pdf",
       width = 9, height = 4.5)


# expressed vote choice conditional on age

dat_age_vote <- dat %>% 
    filter(!is.na(age)) %>% 
    filter(!is.na(firstPrefRec)) %>% 
    group_by(age, firstPrefRec) %>% 
    count() %>% 
    group_by(age) %>% 
    mutate(n_valid = sum(n),
           prop = n / n_valid,
           ci_lower90 =  prop - 1.645 * sqrt((prop * (1 - prop))/ n_valid),
           ci_upper90 =  prop + 1.645 * sqrt((prop * (1 - prop))/ n_valid),
           ci_lower95 =  prop - 1.96 * sqrt((prop * (1 - prop))/ n_valid),
           ci_upper95 =  prop + 1.96 * sqrt((prop * (1 - prop))/ n_valid))

dat_age_vote$firstPrefRec <- factor(dat_age_vote$firstPrefRec,
                                    levels = factors_party_mip)




# Figure A03 ----
ggplot(data = dat_age_vote, 
       aes(x = forcats::fct_rev(firstPrefRec),
           y = prop,
           fill = firstPrefRec)) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = ci_lower95,
                      ymax = ci_upper95),
                  colour = "grey50",
                  width = 0.5) +
    facet_grid(age~.) +
    scale_fill_manual(values = rev(colours_party_mip)) +
    coord_flip() + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       limits = c(0, 0.4), 
                       breaks = c(seq(0, 0.4, 0.1))) +
    labs(x = NULL, y = "Percent (First-Preference Vote Choice)") +
    theme(legend.position = "none")
ggsave("fig_a03.pdf",
       width = 9, height = 9)



## Figure A04 ----

# repeat left-right over-time analysis for cohorts/generations
dat_ire_cohort_sum <- dat_ire %>%
    filter(!is.na(generation)) %>% 
    filter(between(year_of_birth, 1945, 1996)) %>% 
    group_by(year, generation) %>%
    do(data.frame(rbind(Hmisc::smean.cl.boot(.$left_right0to10)))) 

# select time since 1995 to make estimates comparable
dat_ire_cohort_sum <- dat_ire_cohort_sum %>% 
    mutate(year = as.numeric(year)) %>% 
    filter(year >= 1995)


ggplot(dat_ire_cohort_sum, 
       aes(x = year, y = Mean, 
           ymin = Lower, ymax = Upper)) +
    facet_wrap(~generation, nrow = 1) +
    geom_point(size = 1.5, alpha = 0.8) +
    geom_linerange(alpha = 0.8) +
    geom_smooth(alpha = 0.2, colour = "grey50") + 
    scale_colour_manual(values = c("black", "grey50")) +
    labs(x = "Survey Year", y = "Left-Right Self-Placement")  +
    scale_x_continuous(breaks = c(seq(1970, 2020, 5)),
                       limits = c(1990, 2020)) +
    labs(x = "Survey Year", y = "Left-Right Self-Placement") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
ggsave("fig_a04.pdf",
       width = 9, height = 4)



# repeat with age categories
dat_ire_age_sum <- dat_ire %>%
    group_by(year, age_cat) %>%
    do(data.frame(rbind(Hmisc::smean.cl.boot(.$left_right0to10))))

## Figure A05 ----
ggplot(dat_ire_age_sum, 
       aes(x = year, y = Mean, 
           ymin = Lower, ymax = Upper)) +
    facet_wrap(~age_cat, nrow = 1) +
    geom_point(size = 1.5, alpha = 0.8) +
    geom_linerange(alpha = 0.8) +
    geom_smooth(alpha = 0.2, colour = "grey50") + 
    scale_colour_manual(values = c("black", "grey50")) +
    labs(x = "Survey Year", y = "Left-Right Self-Placement")  +
    scale_x_continuous(breaks = c(seq(1970, 2020, 5)),
                       limits = c(1990, 2020)) +
    labs(x = "Survey Year", y = "Left-Right Self-Placement") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
ggsave("fig_a05.pdf",
       width = 9, height = 5)



## Figure A06 ----
# compare countries

table(data_surveys_1973_2020$country)

dat_countries_select <- data_surveys_1973_2020 %>% 
    mutate(country_recoded = dplyr::recode(
        country, "United Kingdom" = "UK"
    )) %>%     
    group_by(country_recoded) %>% 
    mutate(min_year = min(year, na.rm = TRUE)) %>% 
    filter(min_year <= 1980) 


# select Eurobarometer surveys

dat_countries_sum <- data_surveys_1973_2020 %>% 
    group_by(country, year) %>%
    filter(str_detect(dataset, "barometer") | str_detect(dataset, "eb trendfile")) %>% 
    filter(!is.na(left_right0to10)) %>% 
    summarise(mean = mean(left_right0to10, na.rm = TRUE),
              n = n(),
              se = sd(left_right0to10, na.rm = TRUE) / sqrt(n()),
              ci_lower_90 = mean - 1.645 * se,
              ci_upper_90 = mean + 1.645 * se,
              ci_lower_95 = mean - 1.96 * se,
              ci_upper_95 = mean + 1.96 * se)


# dummy for Ireland
dat_countries_sum <- dat_countries_sum %>% 
    mutate(ireland_dummy = ifelse(country == "Ireland", TRUE, FALSE)) 

# Ireland as first country in graph
dat_countries_sum$country <- relevel(factor(dat_countries_sum$country),
                                     ref = "Ireland")

## Figure A06 ----
ggplot(dat_countries_sum, aes(x = year, y = mean, 
                              ymin = ci_lower_95, ymax = ci_upper_95,
                              colour = ireland_dummy)) +
    geom_smooth(alpha = 0.2, colour = "grey30") + 
    geom_point(size = 1.5, alpha = 0.8) +
    facet_wrap(~country, nrow = 2) +
    geom_linerange(alpha = 0.8) +
    scale_colour_manual(values = c("grey50", "darkgreen")) +
    labs(x = "Survey Year", y = "Left-Right Self-Placement")  +
    labs(x = "Survey Year", y = "Left-Right Self-Placement") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
          legend.position = "none")
ggsave("fig_a06.pdf",
       width = 9, height = 6)


## Figure A07 -----

# repeat average left-right self-placements for all Eurobarometer surveys
dat_ire_eurobarometer_sum <- dat_ire %>%
    filter(str_detect(dataset, "Eurobarometer ")) %>% 
    mutate(eurobar_number = str_replace_all(dataset, "Eurobarometer ", "")) %>% 
    group_by(year, eurobar_number) %>%
    do(data.frame(rbind(Hmisc::smean.cl.boot(.$left_right0to10))))

# only select surveys since 2002
dat_ire_eurobarometer_sum_plot <- dat_ire_eurobarometer_sum %>% 
    mutate(year_eurobar = paste0(year, " (", eurobar_number, ")")) %>% 
    filter(year >= 2002)

nrow(dat_ire_eurobarometer_sum_plot)
# 85 Eurobarometer surveys for Ireland, 2002-2020

ggplot(dat_ire_eurobarometer_sum_plot,
       aes(x = eurobar_number, y = Mean, 
           ymin = Lower, ymax = Upper)) +
    geom_linerange(alpha = 0.8, 
                   position = position_dodge(width = 0.5)) +
    geom_point(size = 2,
               position= position_dodge(width = 0.5)) +
    scale_colour_manual(values = rep("black", 86)) +
    facet_grid(.~year, scales = "free_x", space = "free") +
    labs(x = "Eurobarometer Survey ID", y = "Left-Right Self-Placement") +
    scale_y_continuous(limits = c(4, 6)) +
    theme(legend.position = "none",
          strip.text = element_text(angle = 90, vjust = 0.5, size = 10),
          axis.text.x = element_text(angle = 90, vjust = 0.5, size = 7),
          panel.border = element_rect(fill=NA,color="grey70", size=0.5,
                                      linetype="solid"))
ggsave("fig_a07.pdf",
       width = 19, height = 6)



# create histogram of left-right self-placements in each election study

left_right_self_distributions <- dat_electionstudies %>%
    filter(!is.na(left_right_self)) %>% 
    group_by(year, left_right_self) %>% # group by position and year
    count() %>% 
    group_by(year) %>% 
    mutate(prop = n / sum(n))

# get averages for each year
left_right_self_means <- dat_electionstudies %>% 
    filter(!is.na(left_right_self)) %>% 
    group_by(year) %>% 
    summarise(lr_mean = mean(left_right_self))

left_right_self_means

## Figure A08 ----
ggplot(left_right_self_distributions, 
       aes(x = left_right_self, y = prop)) +
    geom_bar(stat = "identity",
             fill = "grey80") +
    facet_wrap(~year, nrow = 1) +
    scale_x_continuous(breaks = c(seq(0, 10, 2))) +
    scale_y_continuous(labels = scales::percent_format(accuracy =1)) +
    geom_vline(data = left_right_self_means,
               aes(xintercept = lr_mean),
               linetype = "dashed",
               colour = "red") +
    labs(x = "Left-Right Self-Placement",
         y = NULL)
ggsave("fig_a08.pdf",
       width = 9, height = 4)



# load ESRI survey and create histogram comparable to Figure A08

dat_esri <- read.csv("data_esri.csv")


table(dat_esri$lab)


# They asked to place themselves on a scale from 1 ‘Social Liberal’ to 11 
# ‘Social Conservative’ and from 1 ‘Economic Left’ to 11 ‘
# Economic Right’, respectively.


dat_esri_long <- dat_esri %>% 
    select(pol_econ, lab) %>% 
    gather(var, value, -c(lab)) %>% 
    mutate(value = value - 1) %>% 
    mutate(lab = car::recode(lab, "0='Online';1='Lab'")) %>% 
    mutate(question = car::recode(var, "'pol_soc'='Left-right (social)';
                                'pol_econ'='Left-right (economic)'"))


ire_esri_lr_sum <- dat_esri_long %>% 
    filter(!is.na(value)) %>% 
    group_by(lab, question, value) %>% 
    summarise(n = n()) %>%
    mutate(freq = n / sum(n))


ire_esri_lr_means <- dat_esri_long %>% 
    filter(!is.na(value)) %>% 
    group_by(lab, question) %>% 
    summarise(lr_mean = mean(value))

## Figure A09 ----
ggplot(ire_esri_lr_sum, aes(x = as.numeric(value), 
                            y = freq, fill = question)) +
    geom_bar(stat = "identity", fill = "grey70") +
    facet_wrap(~lab) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_x_continuous(breaks = c(seq(0, 10, 2))) +
    geom_vline(data = ire_esri_lr_means, aes(xintercept = lr_mean),
               colour = "red", linetype = "dashed") +
    labs(x = "Left-Right Self-Placement", y = NULL) +
    theme(legend.position = "none")
ggsave("fig_a09.pdf", width = 9, height = 4)




# left-right self-placements by first-preference vote choice in 2020
# after excluding respondents who placed themselves at 0

left_right_self_means_no0 <- dat_all_elections_subset %>% 
    filter(!is.na(left_right_self)) %>% 
    filter(left_right_self != 0) %>% # exclude respondents who placed themselves at 0
    group_by(year) %>% 
    summarise(lr_mean = mean(left_right_self))

left_right_self_means_no0


left_right_self_partymeans_no0 <- dat_all_elections_subset %>%
    filter(!is.na(party_vote_recoded_precise)) %>% 
    filter(left_right_self != 0) %>% # remove respondents who placed themselves at 0
    srvyr::as_survey_design() %>% 
    group_by(year, party_vote_recoded_precise) %>%
    summarise(lr_mean = srvyr::survey_mean(left_right_self, 
                                           na.rm = TRUE)) %>% 
    mutate(lr_ci_95_lower = lr_mean - 1.96 * lr_mean_se) %>% 
    mutate(lr_ci_95_upper = lr_mean + 1.96 * lr_mean_se) %>% 
    mutate(lr_ci_90_lower = lr_mean - 1.645 * lr_mean_se) %>% 
    mutate(lr_ci_90_upper = lr_mean + 1.645 * lr_mean_se) 

# relevel factor
left_right_self_partymeans_no0$party_vote_recoded_precise <- factor(
    left_right_self_partymeans_no0$party_vote_recoded_precise,
    levels = c("Fine Gael", 
               "Fianna Fáil",
               "Labour Party",
               "Green Party", 
               "Sinn Féin",
               "Social Democrats", 
               "Solidarity PBP"))

colours_party <- c("#009FF3",
                   "#66BB66",
                   "#CC0000",
                   "#99CC33",
                   "#326760",
                   "#752F8B",
                   "#660000")


## Figure A10 ----

ggplot(left_right_self_partymeans_no0, 
       aes(x = forcats::fct_rev(as.factor(year)), 
           y = lr_mean,
           colour = party_vote_recoded_precise)) +
    geom_point(size = 2) +
    geom_linerange(aes(ymin = lr_ci_90_lower,
                       ymax = lr_ci_90_upper),
                   size = 1.05) +
    geom_linerange(aes(ymin = lr_ci_95_lower,
                       ymax = lr_ci_95_upper)) +
    coord_flip() +
    scale_y_continuous(limits = c(0, 10), 
                       breaks = c(seq(0, 10, 1))) +
    scale_colour_manual(values = colours_party) +
    facet_grid(party_vote_recoded_precise ~.) +
    theme(legend.position = "none",
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 12),
          strip.text.y = element_text(angle = 0, hjust = 0, size = 12))  +
    labs(x = "Election",  
         y = "Average Left-Right Self-Placement")
ggsave("fig_a10.pdf",
       width = 9, height = 7.5)




# get average left-right self-placements conditional on income category
left_right_self_incomemeans <- dat_electionstudies %>%
    filter(!is.na(left_right_self)) %>% 
    filter(!is.na(income_harmonised)) %>% 
    srvyr::as_survey_design() %>% 
    group_by(year, income_harmonised) %>%
    summarise(lr_mean = srvyr::survey_mean(left_right_self, 
                                           na.rm = TRUE)) %>% 
    mutate(lr_ci_95_lower = lr_mean - 1.96 * lr_mean_se) %>% 
    mutate(lr_ci_95_upper = lr_mean + 1.96 * lr_mean_se) %>% 
    mutate(lr_ci_90_lower = lr_mean - 1.645 * lr_mean_se) %>% 
    mutate(lr_ci_90_upper = lr_mean + 1.645 * lr_mean_se) 


# nicer labels for plot
left_right_self_incomemeans <- left_right_self_incomemeans %>% 
    mutate(income_plot = paste0("Income Category: ", income_harmonised)) %>% 
    mutate(income_plot = ifelse(str_detect(income_plot, "1"),
                                "Income Category: 1 (Lowest)",
                                ifelse(str_detect(income_plot, "5"), "Income Category: 5 (Highest)",
                                       income_plot)))

## Figure A11 ----
ggplot(left_right_self_incomemeans, aes(x = forcats::fct_rev(as.factor(year)), 
                                        y = lr_mean,
                                        colour = income_plot)) +
    geom_point(size = 2) +
    geom_linerange(aes(ymin = lr_ci_90_lower,
                       ymax = lr_ci_90_upper),
                   size = 1.05) +
    geom_linerange(aes(ymin = lr_ci_95_lower,
                       ymax = lr_ci_95_upper)) +
    coord_flip() +
    scale_y_continuous(limits = c(3, 8)) +
    scale_colour_manual(values = c("grey60", "grey50",
                                   "grey40", "grey30",
                                   "black")) +
    facet_grid(income_plot ~.) +
    theme(legend.position = "none",
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 12),
          strip.text.y = element_text(angle = 0, hjust = 0, size = 12))  +
    labs(x = "Election",  
         y = "Average Left-Right Self-Placement")
ggsave("fig_a11.pdf",
       width = 9, height = 4.5)



# repeat Figure 4 from paper but exclude pensioners



## rerun models from Table 1, but remove retired respondents

lm_lr_no_retired_02 <- update(lm_lr_02,  
                              data = filter(dat_reg,
                                            year == "2002" & 
                                                retired == FALSE))

lm_lr_no_retired_07 <- update(lm_lr_02,  
                              data = filter(dat_reg,
                                            year == "2007" & 
                                                retired == FALSE))

lm_lr_no_retired_11 <- update(lm_lr_02,  
                              data = filter(dat_reg,
                                            year == "2011" & 
                                                retired == FALSE))

lm_lr_no_retired_16 <- update(lm_lr_02,  
                              data = filter(dat_reg,
                                            year == "2016" & 
                                                retired == FALSE))

lm_lr_no_retired_20 <- update(lm_lr_02,  
                              data = filter(dat_reg,
                                            year == "2020" & 
                                                retired == FALSE))

# get expected values 
pred_income_02_no_retired <- ggpredict(lm_lr_no_retired_02, terms = c("income_harmonised"),
                                       condition = c(
                                           age_cat = "35-44",
                                           gender = "Male",
                                           urban = "0",
                                           university_degree = "0")) %>% mutate(model = "2002")
pred_income_02_no_retired

pred_income_07_no_retired <- ggpredict(lm_lr_no_retired_07, terms = c("income_harmonised"),
                                       condition = c(
                                           age_cat = "35-44",
                                           gender = "Male",
                                           urban = "0",
                                           university_degree = "0")) %>% mutate(model = "2007")


pred_income_11_no_retired <- ggpredict(lm_lr_no_retired_11, terms = c("income_harmonised"),
                                       condition = c(
                                           age_cat = "35-44",
                                           gender = "Male",
                                           urban = "0",
                                           university_degree = "0")) %>% mutate(model = "2011")

pred_income_16_no_retired <- ggpredict(lm_lr_no_retired_16, terms = c("income_harmonised"),
                                       condition = c(
                                           age_cat = "35-44",
                                           gender = "Male",
                                           urban = "0",
                                           university_degree = "0")) %>% mutate(model = "2016")


pred_income_20_no_retired <- ggpredict(lm_lr_no_retired_20, terms = c("income_harmonised"),
                                       condition = c(
                                           age_cat = "35-44",
                                           gender = "Male",
                                           urban = "0",
                                           university_degree = "0")) %>% mutate(model = "2020")


pred_income_no_retired <- bind_rows(pred_income_02_no_retired,
                                    pred_income_07_no_retired,
                                    pred_income_11_no_retired,
                                    pred_income_16_no_retired,
                                    pred_income_20_no_retired)


pred_income_no_retired <- pred_income_no_retired %>% 
    mutate(income_cat = dplyr::recode(
        x, "1" = "1: Lowest", "5" = "5: Highest"
    ))


## Figure A12 ----
ggplot(pred_income_no_retired, aes(x = predicted, y = x)) +
    geom_point(size = 3) +
    geom_errorbarh(aes(xmin = predicted - 1.96 * std.error,
                       xmax = predicted + 1.96 * std.error),
                   size = 0.5, height = 0) +
    geom_errorbarh(aes(xmin = predicted - 1.645 * std.error,
                       xmax = predicted + 1.645 * std.error),
                   size = 1.3, height = 0) +
    coord_flip() +
    facet_wrap(~model, nrow = 1) +
    scale_x_continuous(limits = c(3, 8)) +
    labs(x = "Expected Left-Right Self-Placement",
         y = "Income Category (from Lowest to Highest)")
ggsave("fig_a12.pdf", 
       width = 9, height = 5)



# run linear regressions with taxes/spending and income differences
# as dependent variables

lm_taxesspend_20 <- lm(taxes_spending ~ gender + urban +
                           university_degree  + 
                           age_cat +
                           income_harmonised,
                       weight = weights,
                       data = filter(dat_reg,
                                     year == "2020"))


lm_incomediff_20 <- lm(income_differences ~  +
                           age_cat +
                           income_harmonised + 
                           gender + urban +
                           university_degree,
                       weight = weights,
                       data = filter(dat_reg,
                                     year == "2020"))






## Table A1 ----
screenreg(list(
    lm_lr_20,
    lm_incomediff_20,
    lm_taxesspend_20
))

wordreg(list(lm_lr_20,
             lm_incomediff_20,
             lm_taxesspend_20),
        single.row = FALSE,
        custom.coef.names = c(
            "(Intercept)",
            "Income category: 2 (ref.: 1)",
            "Income category: 3",
            "Income category: 4",
            "Income category: 5",
            "Age: 25-34 (ref.: 18-24)",
            "Age: 35-44",
            "Age: 45-54",
            "Age: 55-64",
            "Age: 65+",
            "Female",
            "Urban constituency",
            "University degree"),
        size = "footnotesize",
        custom.model.names = c("M1: Left-right", 
                               "M2: Income Diff.",
                               "M3: Taxes and Spending"),
        file = "tab_a01.doc")



## save models for Figure 4 for a regression table



factors_party <- c("Fianna Fáil", "Fine Gael",
                   "Sinn Féin",
                   "Greens and Left bloc",
                   "Other and Independents")

dat_reg_multinom$party_vote <- factor(
    dat_reg_multinom$party_vote,
    levels = factors_party
)



lm_multinom_02 <- multinom(party_vote ~ left_right_self +
                               income_harmonised + age_cat  + 
                               gender + 
                               urban + university_degree,
                           weight = weights,
                           data = filter(dat_reg_multinom,
                                         year == "2002"))

lm_multinom_07 <- update(lm_multinom_02,
                         data = filter(dat_reg_multinom,
                                       year == "2007"))


lm_multinom_11 <- update(lm_multinom_02,
                         data = filter(dat_reg_multinom,
                                       year == "2011"))

lm_multinom_16 <- update(lm_multinom_02,
                         data = filter(dat_reg_multinom,
                                       year == "2016"))


lm_multinom_20 <- update(lm_multinom_02,
                         data = filter(dat_reg_multinom,
                                       year == "2020"))


## Table A2 ----
screenreg(list(lm_multinom_02,
               lm_multinom_07,
               lm_multinom_11,
               lm_multinom_16,
               lm_multinom_20))

coefs <- c("Intercept",
           "Left-right self-placement",
           "Income category: 2 (ref.: 1)",
           "Income category: 3",
           "Income category: 4",
           "Income category: 5",
           "Age: 25-34 (ref.: 18-24)",
           "Age: 35-44",
           "Age: 45-54",
           "Age: 55-64",
           "Age: 65+",
           "Female",
           "Urban constituency",
           "University degree")


coefs_multinom <- c(
    paste0("Fine Gael: ", coefs),
    paste0("Sinn Féin: ", coefs),
    paste0("Greens and Left bloc: ", coefs),
    paste0("Other/Ind: ", coefs))


wordreg(list(lm_multinom_02,
             lm_multinom_07,
             lm_multinom_11,
             lm_multinom_16,
             lm_multinom_20),
        single.row = FALSE,
        ci.force = FALSE,
        custom.coef.names = coefs_multinom,
        size = "footnotesize",
        custom.model.names = c("2002", "2007", "2011", "2016", "2020"),
        file = "tab_a02.doc")




## Table A3 ----

# models are run above (for Figures 5 and 6)
screenreg(list(
    multinom_20_incomediff,
    multinom_20_taxespend
))


coefs_20 <- c("Intercept",
              "Income differences",
              "Income category: 2 (ref.: 1)",
              "Income category: 3",
              "Income category: 4",
              "Income category: 5",
              "Age: 25-34 (ref.: 18-24)",
              "Age: 35-44",
              "Age: 45-54",
              "Age: 55-64",
              "Age: 65+",
              "Female",
              "Urban constituency",
              "University degree")


coefs_multinom_20 <- c(
    paste0("Fine Gael: ", coefs_20),
    paste0("Sinn Féin: ", coefs_20),
    paste0("Greens and Left bloc: ", coefs_20),
    paste0("Other and Independent: ", coefs_20))


coefs_multinom_20 <- c(coefs_multinom_20,
                       "Fine Gael: Taxes and Spending",
                       "Sinn Féin: Taxes and Spending",
                       "Greens and Left bloc: Taxes and Spending",
                       "Other and Independents: Taxes and Spending"
)

wordreg(list(multinom_20_incomediff,
             multinom_20_taxespend),
        single.row = FALSE,
        ci.force = FALSE,
        custom.model.names = c("M1: Income Diff.",
                               "M2: Taxes and Spending"),
        custom.coef.names = coefs_multinom_20,
        size = "footnotesize",
        file = "tab_a03.doc")

# random forest models


## select relevant variables for random forest models

dat_random_forest <- dat_electionstudies %>% 
    ungroup() %>% 
    select(year, age_cat, 
           party_vote,
           income_harmonised, urban, gender, 
           university_degree, 
           left_right_self) 

table(dat_random_forest$year)


# run random forest model

year <- unique(dat_random_forest$year)

# store coefficients
dat_importance_merged_lr <- data.frame()

for (i in year) {
    
    dat_year <- filter(dat_random_forest, year == i)
    
    random_forest_year <- randomForest::randomForest(
        left_right_self ~ 
            age_cat + income_harmonised + 
            university_degree +
            urban + gender,
        data = dat_year,
        na.action = na.omit,
        keep.forest = FALSE,
        importance = TRUE,
        ntree = 1000)
    
    
    ## tidy output of random forest model
    dat_importance <- randomForest::importance(random_forest_year, 
                                               type = 1)
    vnames_1 <- rownames(dat_importance)
    i_1 <- as.vector(dat_importance)
    names(i_1) <- vnames_1
    dat_importance <- data.frame(importance = i_1[order(i_1, decreasing = TRUE)])
    dat_importance$variable <- rownames(dat_importance)
    dat_importance$year <- i
    
    dat_importance_merged_lr <- bind_rows(dat_importance,
                                          dat_importance_merged_lr)
    
    
    dat_importance_merged_lr <- dat_importance_merged_lr %>% 
        mutate(variable = dplyr::recode(variable,
                                        "university_degree" = "University Degree",
                                        "income_harmonised" = "Income",
                                        "age_cat" = "Age",
                                        "gender" = "Gender",
                                        "urban" = "Urban/Rural"
        ))
}


# Figure A13 ----
ggplot(data = dat_importance_merged_lr, 
       aes(x = factor(nrow(dat_importance_merged_lr):1),
           y = importance)) +
    geom_bar(stat = "identity", width = 0.1) +
    geom_point(size = 3) +
    facet_wrap(~ year, scales = "free_y", nrow = 5) +
    coord_flip() +
    scale_x_discrete(breaks = nrow(dat_importance_merged_lr):1,
                     labels = dat_importance_merged_lr$variable) +
    labs(x = NULL, y = "Variable Importance (Mean Decrease in Accuracy)")
ggsave("fig_a13.pdf",
       width = 9, height = 7)

