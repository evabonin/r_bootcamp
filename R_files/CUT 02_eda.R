
#' # Title

#+ libraries



# Describe data set: summary

#+
summary(suicide_final)


# heatmap for 2021







#' # Creating some data frames for plotting

# Summarise suicide data over time by country, male and female separately

#+ filtered
sui_country <- suicide_final %>% 
  group_by(country, sex) %>% 
  summarise(mean_sui=mean(sui),
            .groups = 'drop')
print(sui_country, n=50)









# save the widget
# library(htmlwidgets)
# saveWidget(p, file=paste0( getwd(), "/HtmlWidget/ggplotlyAreachart.html"))





#' #' Covariation
#' # Categorical and continuous var
#' ggplot(data = diamonds, mapping = aes(x = price)) + 
#'   geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)
#' 
#' # To make the comparison easier we need to swap what is displayed on the y-axis. Instead of displaying count, we’ll display density, which is the count standardised so that the area under each frequency polygon is one.
#' 
#' ggplot(data = diamonds, mapping = aes(x = price, y = ..density..)) + 
#'   geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)






#' Economic data
#+ case4
switzerland <- suicide_final %>% filter(country == "Switzerland")
lesotho <- suicide_final %>% filter(country == "Lesotho")
barbados <- suicide_final %>% filter(country == "Barbados")

# heat maps??



