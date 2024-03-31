# Model with interaction term for taxes and spending
lm_taxesspend_Int_20 <- lm(taxes_spending ~ urban * age_cat +
                         gender + university_degree + urban * income_harmonised,
                       weight = weights,
                       data = filter(dat_reg, year == "2020"))

lm_taxesspend_Int_20 

# Model with interaction term for income differences
lm_incomediff__Int_20 <- lm(income_differences ~ urban * age_cat +
                          gender + university_degree + urban * income_harmonised,
                       weight = weights,
                       data = filter(dat_reg, year == "2020"))

lm_incomediff__Int_20

lm_lr_02 <- lm(left_right_self ~ 
                 income_harmonised  +
                 age_cat +
                 gender + urban +
                 university_degree,
               weight = weights,
               data = filter(dat_reg,
                             year == "2002"))

lm_lr_20 <- update(lm_lr_02,  
                   data = filter(dat_reg,
                                 year == "2020"))

lm_lr_urban_INT <- lm(left_right_self ~ 
                    urban * income_harmonised  +
                    urban * age_cat +
                    gender + university_degree,
                  weight = weights,
                  data = filter(dat_reg, year == "2020"))

lm_lr_urban_INT

anova(lm_lr_20, lm_lr_urban_INT)
anova(lm_taxesspend_20, lm_taxesspend_Int_20)
anova(lm_incomediff_20, lm_incomediff__Int_20)

## Table A1 ----
screenreg(list(
  lm_lr_urban_INT,
  lm_incomediff__Int_20,
  lm_taxesspend_Int_20
))

wordreg(list(lm_lr_urban_INT,
             lm_incomediff__Int_20,
             lm_taxesspend_Int_20),
        single.row = FALSE,
        size = "footnotesize",
        custom.model.names = c("M1: Left-right_INT", 
                               "M2: Income Diff_INT.",
                               "M3: Taxes and Spending_INT"),
        file = "tab_a01INT.doc")







