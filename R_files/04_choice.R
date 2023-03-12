# https://r-graph-gallery.com/time-series.html

# https://cran.r-project.org/web/packages/TSstudio/vignettes/Plotting_Time_Series.html

# Bubble plot
#https://r-graph-gallery.com/320-the-basis-of-bubble-plot.html



# Interactive world map with leaflet and shiny



# Interactive chart of variables: https://ggplot2-book.org/facet.html

# long ver of dataset

case_study_long  <- suicide_final_long %>%
  filter(country %in% cs_countries, sex != "Both")

ggplot(case_study_long %>% filter(country =="Switzerland"), aes(year1, value)) + 
  geom_line() + 
  facet_wrap(~indicator, scales = "free_y", ncol = 1)

