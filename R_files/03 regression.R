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

reg1 <- suicide_final %>% filter(sex != "Both") %>% feols(sui ~ sex + year)
reg2 <- suicide_final %>% filter(sex != "Both") %>% feols(sui ~ sex + year | country)



modelsummary(reg2)

# plot regression lines

reg_plot1 <- suicide_final %>% filter(sex != "Both") %>%
  ggplot(aes(x = sui, y = sex)) +
  geom_point() +
  geom_smooth(method = "lm", fill = NA)

reg_plot1





models <- list(reg1, reg2)

modelsummary(models,
             stars = TRUE)


modelplot(list(reg1, reg2),
          guides(color = guide_legend(reverse = TRUE)))


