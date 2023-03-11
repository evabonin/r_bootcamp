# Fit regression(s)

# Regression diagnostics (esp. visual)


#https://www.datanovia.com/en/blog/how-to-plot-a-smooth-line-using-ggplot2/


summary(suicide_final)


library(fixest)
library(modelsummary)
library(stats)

library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)

# Does the suicide rate increase over time for males or females?

# Simplest model
reg1 <- suicide_final %>% filter(sex != "Both") %>% feols(sui ~ sex + year + sex*year)

# Fixed effect by country
reg2 <- suicide_final %>% filter(sex != "Both") %>% feols(sui ~ sex + year + sex*year | country)

# Including economic data
reg3 <- suicide_final %>% filter(sex != "Both") %>% feols(sui ~ sex + year + sex*year + pop_t + 
                                                            edu + gdp_pc + unem_y + unem_t | country)

# Including mental health data
reg4 <- suicide_final %>% filter(sex != "Both") %>% feols(sui ~ sex + year + sex*year + pop_t 
                                                          + edu + gdp_pc + unem_y + unem_t+ 
                                                            alc + drug + depr + sh | country)

# 4 plus fixed effect for year
reg5 <- suicide_final %>% filter(sex != "Both") %>% feols(sui ~ sex + pop_t 
                                                          + edu + gdp_pc + unem_y + unem_t+ 
                                                            alc + drug + depr + sh | country + year,
                                                          cluster = ~ country)
# 5 minus vars with zero coefficient
reg6 <- suicide_final %>% filter(sex != "Both") %>% feols(sui ~ sex + 
                                                          edu + unem_y + unem_t+ 
                                                            alc + drug + depr + sh | country + year,
                                                          cluster = ~ country)


models <- list(reg1, reg2, reg3, reg4, reg5, reg6)

coefs = c("sexMale" = "Male", "year" = "Year", "sexMale year" = "Male * Year", "pop_t" = "Total population", "edu" = "Years compulsory education", "gdp_pc" = "GDP per capita", "unem_y" = "Youth unemployment", "unem_t" = "Total unemployment", "alc" = "Alcohol misuse", "drug" = "Drug misuse", "sh" = "Self harm")

modelsummary(models,
             coef_omit = "Intercept",
             coef_map = coefs,
             stars = TRUE)


modelplot(models,
          coef_map = c("sexMale" = "Male")) +
          guides(color = guide_legend(reverse = TRUE))



