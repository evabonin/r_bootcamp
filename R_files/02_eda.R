

# Describe data set: summary


https://r4ds.had.co.nz/exploratory-data-analysis.html

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
