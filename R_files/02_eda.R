

library(BBmisc)
library(tidyverse)
library(table1)


# Describe data set: summary



summary(suicide_final)

# table1 <- table1(~ f_teneviv + s_utilities + s_durables + s_infraest_hh + s_age_sorteo + 
#                    f_sexo + s_yrs + f_single + s_edadhead + s_yrshead + s_tpersona + 
#                    s_num18 + f_estrato + s_puntaje + s_ingtotal | ~ factor(T1T2T3), data=filtered_barrera)

# colnames(suicide_final)


table1 <- suicide_final %>% filter(sex == "Both") %>% 
                                     table1(~ sui + deaths  + pop_t + edu + gdp_pc + unem_y + unem_t + 
                                              alc + drug + depr + sh | factor(continent))
table1


indicators <- c("gdp_pc", "edu", "sui_female", "sui_male", "sui", "unem_y_female", "unem_y_male", "unem_y", "unem_t_female", "unem_t_male", "unem_t", "pop_t")

suicide_final_long <- suicide_final %>%
  pivot_longer(cols = c(5:14),
               names_to = "indicator",
               values_to = "value") 



###################### Creating some data frames for plotting

# Summarise suicide data over time by country, male and female separately

sui_country <- suicide_final %>% 
  group_by(country, sex) %>% 
  summarise(mean_sui=mean(sui),
            .groups = 'drop')
print(sui_country, n=50)


# World map to vizualise (interactive?)
# --> chapter of choice


# Show countries with the highest average suicide rates over time
sui_country_time <- suicide_final %>% 
  group_by(country, sex) %>% 
  summarise(mean_sui=mean(sui),
            .groups = 'drop')
summary(sui_country_time)


# Sorting

sui_country_time_sorted <- sui_country_time[order(sui_country_time$mean_sui, decreasing = TRUE),]
print(sui_country_time_sorted, n = 20)

# All males.

dim((sui_country_time_sorted))

# Max only for males
head(sui_country_time_sorted %>% filter(sex == "Male"), n=1)

# Min only for males
tail(sui_country_time_sorted %>% filter(sex == "Male" & mean_sui > 0), n=1)

# Value for Switzerland
print(sui_country_time_sorted %>% filter(sex == "Male" & country == "Switzerland"))

# --> Selecting Lesotho, Switzerland and Barbados for case study


# plot these three time series (interactive with ggplotly)

cs_countries <- c("Switzerland", "Lesotho", "Barbados")
case_study <- suicide_final %>%
  filter(country %in% cs_countries, sex != "Both")

p <- ggplot(case_study, aes(x = year1, y = sui, color = country)) +
  geom_line() +
  facet_grid(country ~ sex, scales = "free_y") +
  ylab("Suicide rate") +
  xlab("Year")

p <- ggplotly(p)
p


# Summarise suicide data by continent
sui_continent <- suicide_final %>% 
  group_by(continent, sex, year1) %>% 
  summarise(mean_sui=mean(sui), mean_deaths = mean(deaths),
            .groups = 'drop')

print(sui_continent, n=30)


sui_continent_sorted <- sui_continent %>%
  filter(sex == "Both") %>%
  arrange(desc(mean_sui))





# Then: Graphs by continent

# limiting to last five years

p1 <- suicide_final %>% filter(sex == "Male", year > 2016) %>%
  ggplot(aes(x=year1, y=sui, fill=continent)) + 
  geom_boxplot() +
  facet_wrap(~continent, ncol = 1) +
  xlab("Year") +
  ylab("Suicide rate")

p1 <- ggplotly(p1)
p1



# from: https://r-graph-gallery.com/163-interactive-area-chart-plotly.html

# Libraries
library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)




m_c <- sui_continent %>% ggplot(aes(x=year1,y=mean_sui)) +
  geom_line() +
    facet_grid(sex~continent, scales = "free_y") +
  theme_ipsum() +
  ylab("Suicide rate") +
  xlab("Year")
m_c <- ggplotly(m_c)
m_c



# save the widget
# library(htmlwidgets)
# saveWidget(p, file=paste0( getwd(), "/HtmlWidget/ggplotlyAreachart.html"))


# Histogram: suicide deaths by continent




library(tidyverse)
library(hrbrthemes)


# plot
q <- sui_continent %>% filter(sex != "Both") %>%
  ggplot(aes(x = year1, y=cumsum(mean_deaths), colour = sex)) +
  geom_line() +
  ggtitle("Suicide deaths") +
  theme_ipsum() +
  facet_wrap(~ continent, scales = "free_y")

q <- ggplotly(q)
q



# Then: Case study Switzerland

# suicide rate over time plus cumulative deaths

switzerland <- suicide_final %>% filter(country == "Switzerland")
lesotho <- suicide_final %>% filter(country == "Lesotho")
barbados <- suicide_final %>% filter(country == "Barbados")









# Multiple lines in the same plot
case_study %>% 
  ggplot(mapping = aes(y = sui, x=year, colour = sex)) +
  geom_line() +
  facet_wrap(~ country, scales = "free_y")


#https://r4ds.had.co.nz/exploratory-data-analysis.html

# Bar Plots / histograms: To compare the number of suicide deaths across different demographic or geographical categories.
# Calculate by multiplying rates by total population? For male / female (assume 50% of total pop).

# Categorical
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))

# continuous

ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.5)

# Multiple histograms in the same plot
ggplot(data = smaller, mapping = aes(x = carat, colour = cut)) +
  geom_freqpoly(binwidth = 0.1)

# Heat Maps: To show geographical variations in the number of suicide deaths vs suicide rates

# Time Series Plots: To show trends and patterns in the number of suicide deaths over time. By continent?


# Box Plots: To show the distribution of values for each variable, including median, quartiles, and outliers.
# Density Plots: To show the distribution of values for each variable, and to help identify any patterns in the data.
# Scatter Plots: To identify any relationships or correlations between different variables, such as age and suicide deaths.


#Covariation
# Categorical and continuous var
ggplot(data = diamonds, mapping = aes(x = price)) + 
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)

# To make the comparison easier we need to swap what is displayed on the y-axis. Instead of displaying count, we’ll display density, which is the count standardised so that the area under each frequency polygon is one.

ggplot(data = diamonds, mapping = aes(x = price, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)
#> Warning: The dot-dot notation (`..density..`) was deprecated in ggplot2 3.4.0.
#> ℹ Please use `after_stat(density)` instead.
