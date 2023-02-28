# Loading WB data from web
# useful links:
# https://github.com/gshs-ornl/wbstats
# https://joenoonan.se/post/country-code-tutorial/


library(wbstats)
library(dplyr)
#library(reshape)
library(tidyr)
library(countrycode)
library(ggplot2)




world_bank <- wb_data(country = "countries_only",
                      indicator = c("SP.POP.TOTL", "SH.STA.SUIC.P5" , "SH.STA.SUIC.FE.P5" , "SH.STA.SUIC.MA.P5" , "SE.COM.DURS" , "NY.GDP.MKTP.KD" , "NY.GDP.PCAP.KD" , "SL.UEM.1524.ZS" , "SL.UEM.1524.MA.ZS" , "SL.UEM.1524.FE.ZS" , "SL.UEM.TOTL.ZS" , "SL.UEM.TOTL.MA.ZS" , "SL.UEM.TOTL.FE.ZS"),
                      mrv = 5,
                      gapfill = TRUE) %>% 
  filter(date >= 2000)


varnames <- c("ISO2C", "ISO3C", "Country", "Date", "GDP (constant 2015 US$)", "GDP per capita (constant 2015 US$)", "Compulsory education, duration (years)" , 
              "Suicide mortality rate (per 100,000 population) female", "Suicide mortality rate (per 100,000 population) male", "Suicide mortality rate (per 100,000 population)", 
              "Unemployment, youth total (% of total labor force ages 15-24) (modeled ILO estimate) female" ,   "Unemployment, youth total (% of total labor force ages 15-24) (modeled ILO estimate) male" , 
              "Unemployment, youth total (% of total labor force ages 15-24) (modeled ILO estimate)" ,
              "Unemployment, total (% of total labor force) (modeled ILO estimate) female", "Unemployment, total (% of total labor force) (modeled ILO estimate) male",
              "Unemployment, total (% of total labor force) (modeled ILO estimate)", "Total population")


colnames(world_bank) <- varnames


#world_bank_long <- melt(world_bank, id=c("country","iso3c", "date"), variable_name = "indicator")


wb_long <- world_bank %>%
  pivot_longer(cols = !c(Country, ISO3C, ISO2C, Date),
               names_to = "Indicator",
               values_to = "Value"
               )


# Creating gender column in WB data

library(stringr)



wb_long <- wb_long %>%
  mutate(Sex = case_when(
    str_detect(Indicator, "female") ~ "Female",
    str_detect(Indicator, "male") ~ "Male"),
    Sex = ifelse(is.na(Sex), "Both", Sex))

# Remove "male" and "female" from indicator descriptions

wb_long$Indicator <- gsub(" male","",as.character(wb_long$Indicator))
wb_long$Indicator <- gsub(" female","",as.character(wb_long$Indicator))

# dropping ISO2C column
wb_long <- subset(wb_long, select = -c(ISO2C))



### GBD dataset ###



# Loading GBD data

gbd <- read.table("../data/gbd/gbd.csv", header = TRUE, sep = ",", na.strings = "..")

## Dropping columns we don't need

gbd <- gbd[, -c(1, 2, 3, 5, 7, 8, 9, 11, 12, 15, 16)]


# insert column with country codes
# converting country names to world bank destination coding scheme using countrycode library. target var is ISO3C

gbd <- gbd %>%
  mutate(ISO3C = countrycode(location_name,"country.name", "wb"))

#check to see if there are any cases missing in ISO3C

filter(gbd, is.na(ISO3C))

# There are a few missing, but will deal with this later


# Renaming columns


gbd_oldnames <- colnames(gbd)
gbd_oldnames
gbd_newnames <- c("Country", "Sex", "Indicator", "Date", "Value", "ISO3C" )
colnames(gbd) <- gbd_newnames




# MERGING DATASETS
wb_gbd_long <- rbind(wb_long, gbd)



# Creating wide version of wb_gbd

wb_gbd_wide <- wb_gbd_long %>%
  pivot_wider(names_from = "Indicator",
               values_from = "Value"
  )


##########################################

# rename the columns using the colnames() function
colnames(wb_gbd_wide) <- c("country_code", "country","year","GDP", "GDP_per_capita","education", "suicide_rate","unemployment_youth", "unemployment_total", "population", "alcohol", "drug", "depression","selfharm", "sexual_violence")

#colnames(Suicide_complete)[5] <- "GDP_per_capita"

# Add continent colume 

continent <- read.csv("../data/countryContinent.csv") 

continent <- continent[, -c( 2, 3, 4, 5, 7, 8, 9)]

Suicide <- left_join(wb_gbd_wide, continent, by = "country")

glimpse(Suicide)

# Delete column index 16 empty column
Suicide <- Suicide[, -16]

glimpse(Suicide)


# STEP 3: VISUALIZE THE DATA



# create histogram of values for total suicide morality rate

ggplot(data = Suicide, aes(x= suicide_rate))+ 
  geom_histogram(fill= "steelblue", color= "black")+
  ggtitle("Histogram of total Suicide mortality rate")



# Calculate the suicide rate for each continent

ggplot(data = Suicide, aes(x = continent, y = suicide_rate, fill = continent)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Suicide Rate by Continent") + 
  xlab("Continent") + 
  ylab("Suicide Rate") + 
  scale_fill_brewer(type = "qual", palette = "Paired")


# Calculate the suicide rate for each country . # TO BE FIXED
print(ggplot(data = Suicide, aes(x = reorder(country, suicide_rate), y = suicide_rate, fill = country)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Suicide Rate by Country") + 
  xlab("Country") + 
  ylab("Suicide Rate") + 
  coord_flip() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)))


###########################################

# Load required libraries
library(tidyverse)


# Remove NA values from unemployment_youth and unemployment_total
complete_data <- Suicide[complete.cases(Suicide[c("unemployment_youth", "unemployment_total")]),]


# Plot suicide rate against unemployment_youth and unemployment_total

plot(complete_data$unemployment_youth, complete_data$suicide_rate, 
     xlab = "Youth Unemployment (%)", ylab = "Suicide Rate (per 100,000 population)", 
     main = "Relationship between Youth Unemployment and Suicide Rate")

plot(complete_data$unemployment_total, complete_data$suicide_rate, 
     xlab = "Total Unemployment (%)", ylab = "Suicide Rate (per 100,000 population)", 
     main = "Relationship between Total Unemployment and Suicide Rate")

# Create a scatterplot to visualize the relationship between suicide and unemployment_total
ggplot(complete_data, aes(x = unemployment_total, y = suicide_rate, color = country)) +
  geom_point() +
  facet_wrap(~ year) +
  labs(x = "Unemployment rate (total)", y = "Suicide rate") +
  theme(legend.position = "none")

#############################################


# Plot the relationship between the suicide rate and GDP per capita
ggplot(Suicide, aes(x=GDP_per_capita, y=suicide_rate)) +
  geom_point() +
  xlab("GDP per capita") +
  ylab("Suicide rate") +
  ggtitle("Relationship between Suicide rate and GDP per capita")

#simple linear regression analysis between the suicide mortality rate and GDP per capita

# Perform a linear regression analysis between the suicide rate and GDP per capita
lm_model <- lm(suicide_rate ~ GDP_per_capita, data = Suicide)

# Print the summary of the model
summary(lm_model)

# Visualize the relationship between suicide rate and GDP per capita using a scatterplot
ggplot(Suicide, aes(x = GDP_per_capita, y = suicide_rate)) + 
  geom_point() + 
  geom_smooth(method = lm) + 
  labs(title = "Relationship between Suicide Rate and GPD per Capita")

##################################################


# Plot the relationship between the suicide rate and unemployment

ggplot(Suicide, aes(x=unemployment_total, y=suicide_rate)) +
  geom_point() +
  xlab("Unemployment") +
  ylab("Suicide rate") +
  ggtitle("Relationship between Suicide rate and Unemployment")

# Remove missing values from Suicide dataset

Suicide_complete <- na.omit(Suicide)

#Correlation analysis: to quantify the strength and direction of the relationship.

# Calculate the Pearson correlation coefficient between the suicide rate and GDP per capita
cor_GDP_per_capita <- cor(Suicide_complete$suicide_rate, Suicide_complete$GDP_per_capita)
cat("The Pearson correlation coefficient between the suicide rate and GDP per capita is:", cor_GDP_per_capita, "\n")

ggplot(Suicide_complete, aes(x = GDP_per_capita, y = suicide_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle(paste("Suicide Rate vs. GDP_per_capita (Pearson Correlation Coefficient =", round(cor_GDP_per_capita, 2), ")"))


# Calculate the Pearson correlation coefficient between the suicide rate and unemployment

cor_unemployment_total <- cor(Suicide_complete$suicide_rate, Suicide_complete$unemployment_total)
cat("The Pearson correlation coefficient between the suicide rate and unemployment is:", cor_unemployment_total, "\n")

# Create the scatter plot correlation coefficient between the suicide rate and unemployment
ggplot(Suicide_complete, aes(x = unemployment_total, y = suicide_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle(paste("Suicide Rate vs. Unemployment Total (Pearson Correlation Coefficient =", round(cor_unemployment_total, 2), ")"))


# Calculate the Pearson correlation coefficient between the suicide rate and alcohol consumption
cor_alcohol <- cor(Suicide_complete$suicide_rate, Suicide_complete$alcohol)
cat("The Pearson correlation coefficient between the suicide rate and alcohol consumption is:", cor_alcohol, "\n")

ggplot(Suicide_complete, aes(x = alcohol, y = suicide_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle(paste("Suicide Rate vs. Alcohol Consumption (Pearson Correlation Coefficient =", round(cor_alcohol, 2), ")"))


# Calculate the Pearson correlation coefficient between the suicide rate and drug use
cor_drug <- cor(Suicide_complete$suicide_rate, Suicide_complete$drug)
cat("The Pearson correlation coefficient between the suicide rate and drug use is:", cor_drug, "\n")

ggplot(Suicide_complete, aes(x = drug, y = suicide_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle(paste("Suicide Rate vs. Drug Use (Pearson Correlation Coefficient =", round(cor_drug, 2), ")"))


# Calculate the Pearson correlation coefficient between the suicide rate and depression
cor_depression <- cor(Suicide_complete$suicide_rate, Suicide_complete$depression)
cat("The Pearson correlation coefficient between the suicide rate and depression is:", cor_depression, "\n")

ggplot(Suicide_complete, aes(x = depression, y = suicide_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle(paste("Suicide Rate vs. Depression (Pearson Correlation Coefficient =", round(cor_depression, 2), ")"))


# Calculate the Pearson correlation coefficient between the suicide rate and self-harm
cor_selfharm <- cor(Suicide_complete$suicide_rate, Suicide_complete$selfharm)
cat("The Pearson correlation coefficient between the suicide rate and self-harm is:", cor_selfharm, "\n")

ggplot(Suicide_complete, aes(x = selfharm, y = suicide_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle(paste("Suicide Rate vs. Self-Harm (Pearson Correlation Coefficient =", round(cor_selfharm, 2), ")"))



# Calculate the Pearson correlation coefficient between the suicide rate and sexual violence
cor_sexual_violence <- cor(Suicide_complete$suicide_rate, Suicide_complete$sexual_violence)
cat("The Pearson correlation coefficient between the suicide rate and sexual violence is:", cor_sexual_violence, "\n")

ggplot(Suicide_complete, aes(x = sexual_violence, y = suicide_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle(paste("Suicide Rate vs. Sexual Violence (Pearson Correlation Coefficient =", round(cor_sexual_violence, 2), ")"))


###########################################

#colnames(Suicide_complete)[5] <- "GDP_per_capita"


#Clustering countries based on their economic indicators and suicide rate to see if there are any patterns or similarities in the relationship.

library(cluster)

# Select relevant variables from the data
cluster_vars <- c("GDP_per_capita", "unemployment_youth", "unemployment_total", "suicide_rate")
cluster_data <- Suicide_complete[, cluster_vars]

# Scale the variables
cluster_data_scaled <- scale(cluster_data)

# Determine the optimal number of clusters using the elbow method
wss <- (nrow(cluster_data_scaled)-1)*sum(apply(cluster_data_scaled,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(cluster_data_scaled, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

# Perform K-means clustering
k = 5 # choose the optimal number of clusters
set.seed(123)
fit <- kmeans(cluster_data_scaled, k, nstart = 25)

# Assign cluster membership to each country
Suicide_complete$cluster <- fit$cluster

# Plot the cluster results
library(ggplot2)
ggplot(Suicide_complete, aes(x = GDP_per_capita, y = suicide_rate, color = factor(cluster))) +
  geom_point() +
  labs(title = "Clustering of countries based on economic indicators and suicide rate",
       x = "GDP per capita",
       y = "Suicide rate") +
  scale_color_discrete(name = "Cluster")


################################################

#global suicide counts by country
#TO BE FIXED

library(tidyverse)

# Read the dataset and filter the data for years between 2017 and 2021
Suicide_filtered <- Suicide %>% 
  filter(year >= 2017 & year <= 2021)


# Group the data by country and continent and calculate the total number of suicides for each country
global_suicide_counts <- Suicide_filtered %>% 
  group_by(country, continent) %>% 
  summarise(total_suicides = sum(suicide_rate, na.rm = TRUE)) %>% 
  arrange(desc(total_suicides))

# Create a bar plot of total suicides by country
ggplot(data = global_suicide_counts, aes(x = reorder(country, total_suicides), y = total_suicides, fill = continent)) +
  geom_bar(stat = "identity") +
  ggtitle("Total Suicides by Country (2017-2021)") +
  xlab("Country") +
  ylab("Total Suicides") +
  coord_flip() +
  scale_fill_manual(values = c("North America" = "blue", "South America" = "green", "Europe" = "red", "Africa" = "orange", "Asia" = "purple", "Oceania" = "brown")) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5))


# View the top 10 countries with the highest suicide counts
head(global_suicide_counts, 10)

