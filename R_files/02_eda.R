
#' # Title

#+ libraries
library(BBmisc)
library(tidyverse)
library(table1)
library(hrbrthemes) # theme ipsom
library(ggplot2)
library(dplyr)
library(plotly)
library(webshot)


# Describe data set: summary

#+
summary(suicide_final)


# heatmap for 2021





#+ table1

tmp_both <- suicide_final %>% filter(sex == "Both")

# table1 <- suicide_final %>% filter(sex == "Both") %>% table1(~ sui + deaths  + pop_t + edu + gdp_pc + unem_y + unem_t + 
#                                               alc + drug + depr + sh | ~ factor(continent))

table1 <- table1(~ sui + deaths + pop_t + edu + gdp_pc + unem_y + unem_t + alc + drug + depr + sh | factor(continent), data = tmp_both)


table1




#' # Creating some data frames for plotting

# Summarise suicide data over time by country, male and female separately

#+ filtered
sui_country <- suicide_final %>% 
  group_by(country, sex) %>% 
  summarise(mean_sui=mean(sui),
            .groups = 'drop')
print(sui_country, n=50)



# Show countries with the highest average suicide rates over time

#+ time

sui_country_time <- suicide_final %>% 
  group_by(country, sex) %>% 
  summarise(mean_sui=mean(sui),
            .groups = 'drop')
#summary(sui_country_time)


# Sorting

sui_country_time_sorted <- sui_country_time[order(sui_country_time$mean_sui, decreasing = TRUE),]
print(sui_country_time_sorted, n = 20)

# All males.

#dim((sui_country_time_sorted))

# Max only for males
head(sui_country_time_sorted %>% filter(sex == "Male"), n=1)

# Min only for males
tail(sui_country_time_sorted %>% filter(sex == "Male" & mean_sui > 0), n=1)

# Value for Switzerland
print(sui_country_time_sorted %>% filter(sex == "Male" & country == "Switzerland"))

#' Selecting Lesotho, Switzerland and Barbados for the case study.




#' Summarise suicide data by continent

#+ continent
sui_continent <- suicide_final %>% 
  group_by(continent, sex, year1) %>% 
  summarise(mean_sui=mean(sui), mean_deaths = mean(deaths),
            .groups = 'drop')

#print(sui_continent, n=30)


sui_continent_sorted <- sui_continent %>%
  filter(sex == "Both") %>%
  arrange(desc(mean_sui))



#' Then: Graphs by continent

#+ box_cont

# limiting to last five years

p1 <- suicide_final %>% filter(sex == "Male", year > 2016) %>%
  ggplot(aes(x=year, y=sui, fill=continent)) + 
  geom_boxplot() +
  facet_wrap(~continent, ncol = 1) +
  xlab("Year") +
  ylab("Suicide rate")

p1 <- ggplotly(p1)
p1



# from: https://r-graph-gallery.com/163-interactive-area-chart-plotly.html

#+ line_cont

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


#' # Suicide deaths by continent

#+ deaths_cont
# plot
q <- sui_continent %>% filter(sex != "Both") %>%
  ggplot(aes(x = year1, y=cumsum(mean_deaths), colour = sex)) +
  geom_line() +
  ggtitle("Suicide deaths") +
  theme_ipsum() +
  facet_wrap(~ continent, scales = "free_y")

q <- ggplotly(q)
q




#' #' Covariation
#' # Categorical and continuous var
#' ggplot(data = diamonds, mapping = aes(x = price)) + 
#'   geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)
#' 
#' # To make the comparison easier we need to swap what is displayed on the y-axis. Instead of displaying count, we’ll display density, which is the count standardised so that the area under each frequency polygon is one.
#' 
#' ggplot(data = diamonds, mapping = aes(x = price, y = ..density..)) + 
#'   geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)




#' # Case study: Switzerland

#+ case1

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


#' suicide rate over time plus cumulative deaths

#+ case2

# Multiple lines in the same plot
case_study %>% 
  ggplot(mapping = aes(y = sui, x=year, colour = sex)) +
  geom_line() +
  facet_wrap(~ country, scales = "free_y")


#+ case3

# Plot above but with line for sub-region???


#' Economic data
#+ case4
switzerland <- suicide_final %>% filter(country == "Switzerland")
lesotho <- suicide_final %>% filter(country == "Lesotho")
barbados <- suicide_final %>% filter(country == "Barbados")

# heat maps??



