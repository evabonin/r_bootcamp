# Load the required packages
library(zoo)
library(dplyr)

set.seed(123)
date <- seq(as.Date("2020-01-01"), as.Date("2022-12-31"), by = "day")
country <- rep(c("A", "B", "C"), length.out = length(date))
sex <- rep(c("F", "M"), length.out = length(date))
value <- rnorm(length(date), mean = 10, sd = 2)
df <- data.frame(date, country, sex, value)

# Impute missing values using linear interpolation by group (country)
df_imputed <- df %>%
  group_by(country, sex) %>%
  arrange(date) %>%
  mutate(value = na.approx(value, na.rm = FALSE))

# Print the imputed data
print(df_imputed)


df_summary <- df %>%
  group_by(country, sex) %>%
  summarize(n = n(), mean_value = mean(value, na.rm = TRUE))
# Print the summary data
print(df_summary)


library(dplyr)

# Create example data frame
df <- data.frame(country = c("USA", "USA", "USA", "Canada", "Canada", "Mexico"),
                 age = c(25, NA, 30, 40, NA, 50),
                 income = c(50000, 60000, NA, 70000, 80000, NA))

# Group by country and summarize missing values in each column
df_miss <- df %>%
  group_by(country) %>%
  summarize(across(everything(), ~sum(is.na(.))))

# Filter only those with missing values
df_miss <- df_miss %>%
  filter(across(-country, ~all(is.na(.))))

# Transpose the table
df_miss <- t(df_miss)

# Rename the row names
rownames(df_miss) <- c("missing_age", "missing_income")

# View the result
df_miss

