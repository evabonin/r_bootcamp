# Loading WB data from web
# useful links:
# https://github.com/gshs-ornl/wbstats
# https://joenoonan.se/post/country-code-tutorial/

library(wbstats)
library(dplyr)
library(reshape)
library(tidyr)
library(countrycode)
library(stringr)
library(imputeTS)
library(writexl)
library(foreach)
library(data.table)

# apply_labels
library(expss)

# Visualising:
library(tidyverse)
library(naniar)
library(ggplot2)


# Using data from 2017 onwards because that seems to avoid a lot of issues around missing values on the economic indicators.

world_bank2 <- wb_data(country = "countries_only",
                      indicator = c("SP.POP.TOTL", "SH.STA.SUIC.P5" , "SH.STA.SUIC.FE.P5" , "SH.STA.SUIC.MA.P5" , "SE.COM.DURS" , "NY.GDP.PCAP.KD" , "SL.UEM.1524.ZS" , "SL.UEM.1524.MA.ZS" , "SL.UEM.1524.FE.ZS" , "SL.UEM.TOTL.ZS" , "SL.UEM.TOTL.MA.ZS" , "SL.UEM.TOTL.FE.ZS"),
                      mrv = 10,
                      gapfill = TRUE) # this automatically does interpolation!
#%>% 
 # filter(date >= 2017)


varnames <- c("iso2c", "iso3c", "country", "year", "gdp_pc", "edu", "sui_female", "sui_male", "sui", "unem_y_female", "unem_y_male", "unem_y", "unem_t_female", "unem_t_male", "unem_t", "pop_t")
colnames(world_bank) <- varnames

# world_bank = apply_labels(world_bank,
#                           iso2c = "ISO2C", 
#                           iso3c = "ISO3C", 
#                           country = "Country", 
#                           year = "Year", 
#                           gdp_pc = "GDP per capita (constant 2015 US$)", 
#                           edu = "Compulsory education, duration (years)",
#                           sui_female = "Suicide mortality rate (per 100,000 population) female", 
#                           sui_male = "Suicide mortality rate (per 100,000 population) male", 
#                           sui = "Suicide mortality rate (per 100,000 population)", 
#                           unem_y_female = "Unemployment, youth total (% of total labor force ages 15-24) (modeled ILO estimate) female", 
#                           unem_y_male = "Unemployment, youth total (% of total labor force ages 15-24) (modeled ILO estimate) male",
#                           unem_y = "Unemployment, youth total (% of total labor force ages 15-24) (modeled ILO estimate)" ,
#                           unem_t_female = "Unemployment, total (% of total labor force) (modeled ILO estimate) female", 
#                           unem_t_male = "Unemployment, total (% of total labor force) (modeled ILO estimate) male",
#                           unem_t = "Unemployment, total (% of total labor force) (modeled ILO estimate)", 
#                           pop_t = "Total population")





#world_bank_long <- melt(world_bank, id=c("country","iso3c", "date"), variable_name = "indicator")


wb_long <- world_bank %>%
  pivot_longer(cols = !c(country, iso3c, iso2c, year),
               names_to = "indicator",
               values_to = "value"
               )

wb_long[wb_long == 'NULL'] <- NA

# Creating gender column in WB data

wb_long <- wb_long %>%
  mutate(sex = case_when(
    str_detect(indicator, "female") ~ "Female",
    str_detect(indicator, "male") ~ "Male"),
    sex = ifelse(is.na(sex), "Both", sex))

# Remove "male" and "female" from indicator descriptions

wb_long$indicator <- gsub("_male","",as.character(wb_long$indicator))
wb_long$indicator <- gsub("_female","",as.character(wb_long$indicator))

# dropping ISO2C column; pretty sure there's an easier way.
wb_long <- subset(wb_long, select = -c(iso2c))




### GBD dataset ###


# Loading GBD data

gbd <- read.csv("../data/gbd/gbd_clean.csv", header = TRUE, sep = ",", na.strings = "NA")



## Dropping columns we don't need

gbd <- gbd[, -c(1, 2, 3, 5, 7, 8, 9, 11, 12, 15, 16)]


# insert column with country codes
# converting country names to world bank destination coding scheme using countrycode library. target var is ISO3C

gbd <- gbd %>%
  mutate(iso3c = countrycode(location_name,"country.name", "wb"))

# Warning message:
#   There was 1 warning in `mutate()`.
# ℹ In argument: `ISO3C = countrycode(location_name, "country.name", "wb")`.
# Caused by warning in `countrycode_convert()`:
#   ! Some values were not matched unambiguously: Cook Islands, Niue, Tokelau, Turkiye 

# Manually adding ISO country code for these:
gbd$iso3c[gbd$location_name == "Cook Islands"] <- "COK"
gbd$iso3c[gbd$location_name == "Niue"] <- "NIU"
gbd$iso3c[gbd$location_name == "Tokelau"] <- "TKL"
gbd$iso3c[gbd$location_name == "Turkiye"] <- "TUR"

#check to see if there are any cases missing in ISO3C
filter(gbd, is.na(iso3c))
# is now complete


# Drop data before 2017 (although this is a shame)
gbd <- gbd[gbd$year >= 2017, ]

# Dropping rows relating to sexual voilence because thta appears to be zero throughout.
gbd <- gbd[gbd$cause_name != "Sexual violence", ] 




# Renaming columns


gbd_oldnames <- colnames(gbd)
gbd_oldnames
gbd_newnames <- c("country", "sex", "indicator", "year", "value", "iso3c" )
colnames(gbd) <- gbd_newnames



# Adding in continent:

continent <- read.csv("../data/continents.csv") 




# MERGING DATASETS
wb_gbd_long <- rbind(wb_long, gbd)
wb_gbd_long[wb_gbd_long == 'NULL'] <- NA





# Creating wide version of wb_gbd

wb_gbd_wide <- wb_gbd_long %>%
  pivot_wider(names_from = "indicator",
               values_from = "value"
  )


wb_gbd_wide <- left_join(wb_gbd_wide, continent, by = "iso3c")





# wb_gbd_long %>%
#   dplyr::group_by(ISO3C, Country, Year, Sex, Indicator) %>%
#   dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
#   dplyr::filter(n > 1L) 


# Checking country names for combined dataset as some variance in spelling was found:

countries_wb_gbd <- as.list(unique(wb_gbd_wide[c("country")]))
countries_wb_gbd <- lapply(countries_wb_gbd, sort, decreasing = FALSE)
countries_wb_gbd <- as.data.frame(countries_wb_gbd)

write_xlsx(countries_wb_gbd, "../outputs/countries_combined.xlsx")

# The following were changed manually in the gbd dataset to reflect spelling in the wb dataset:

# Bahamas
# Bahamas, The
#
# Bolivia
# Bolivia (Plurinational State of)
# 
# Congo
# Congo, Dem. Rep.
# Congo, Rep.
# Democratic Republic of the Congo
# 
# Cote d'Ivoire
# Côte d'Ivoire
# 
#
# Egypt
# Egypt, Arab Rep.
#
# Gambia
# Gambia, The
#
# Iran (Islamic Republic of)
# Iran, Islamic Rep.
#
# Kyrgyz Republic
# Kyrgyzstan
# 
# Lao PDR
# Lao People's Democratic Republic
# 
# Micronesia (Federated States of)
# Micronesia, Fed. Sts.
#
# Slovak Republic
# Slovakia
#
# Turkey
# Turkiye
#
# United States
# United States of America
#
# Venezuela (Bolivarian Republic of)
# Venezuela, RB
# 
# Viet nam
# Vietnam
#
# Yemen
# Yemen, Rep.




# Getting summary of wide dataset

summary(wb_gbd_wide)


# renaming vars so all are in line

varnames_old <- colnames(wb_gbd_wide)
varnames_old
varnames_new <- c("iso3c", "country", "year","sex","gdp_pc","edu","sui","unem_y","unem_t","pop_t","alc", "drug","depr","sh", "continent", "region")
colnames(wb_gbd_wide) <- varnames_new




# Unique values for Year variable --> will be used for filtering and looping.

#years <- as.list(unique(wb_gbd_wide[c("year")]))
#years <- lapply(years, sort, decreasing = FALSE)



# Imputing missing values - My attempt

# Considerations for imputation:
# - GDP, GDP / capita, compulsory education, total population: same value for male / female as overall. Can use na_mean from imputeTS package.
# - Then, for all vars: Missing years --> interpolation
# 
# both <- filter(wb_gbd_wide, wb_gbd_wide$Country == "Aruba" & wb_gbd_wide$Year == "2017" & wb_gbd_wide$Sex == "Both")
# test <- both$`GDP per capita (constant 2015 US$)`
# test
# 
# 
# # This finally works.But need to save it to the dataset somehow and then turn it into a loop... Can add other columns to the list.
# filter(wb_gbd_wide, wb_gbd_wide$Country == "Aruba" & wb_gbd_wide$Year == "2017" & wb_gbd_wide$Sex != "Both") %>% replace_na(list(`GDP per capita (constant 2015 US$)`= test))
# 
# 
# # Another attempt, there seems to be a typo around line 230 but I don't know what it is.
# 
# wb_gbd_wide %>%
#   group_by(wb_gbd_wide$Country) %>%
#   mutate(
#     wb_gbd_wide$`GDP (constant 2015 US$)` = impute.mean(wb_gbd_wide$`GDP (constant 2015 US$)`)
#   )

# Maybe check this approach: https://stackoverflow.com/questions/71756025/impute-missing-values-within-each-group-based-on-an-equation-in-r



# Imputation using approach by Claude


## Prepare second data set
wb_gbd_wide.both <- wb_gbd_wide %>% 
  filter(sex == "Both") %>% 
  select(!sex)

## Join the two data sets because you have to make sure that contry and year
## are correctly merged. 
wb_gbd_wide.tmp2 <- 
  left_join(x = wb_gbd_wide, y = wb_gbd_wide.both, 
            by = c("country", "year"),
            suffix = c("", ".y"))


### finally got this working, I'm sure there is a more elegant way of doing this.
wb_gbd_wide <- wb_gbd_wide.tmp2 %>% 
  mutate(sui = coalesce(sui, sui.y),
         gdp_pc = coalesce(gdp_pc, gdp_pc.y), 
         edu = coalesce(edu, edu.y), 
         pop_t = coalesce(pop_t, pop_t.y))
         
         
# remove columns with .y suffix

columns_to_remove <- grep("\\.y", names(wb_gbd_wide))
wb_gbd_wide <- wb_gbd_wide[,-columns_to_remove]


# list of countries
countries_wb_gbd <- as.list(unique(wb_gbd_wide[c("country")]))

length(countries_wb_gbd[[1]])
# we have 230 countries


# Counting missing values by country

na_count <- wb_gbd_wide %>%
  group_by(country) %>%
  dplyr::summarize(count_na = sum(is.na(sui)))

na_count
print(na_count, n=230)

# barplot(na_count$count_na, names.arg = na_count$country, xlab = "Country", ylab = "Count of NA Values", main = "Count of NA Values by Country")

# count the occurrences of each value in count_na[2]
counts <- table(na_count$count_na)
print(counts)


### THIS ONE FOR REPORT

# create a bar plot of the counts
barplot(counts, xlab = "Number of NA Values", ylab = "Count", main = "Counts of NA Values")

# Which countries are completely missing --> remove from dataset
# subset na_count to include only rows where count_na[2] is 15
subset15 <- na_count[na_count$count_na == 15, ]

# print the values of count_na[1] in the subset
print(subset15, n=34)


# Which countries are missing missing?
# subset na_count to include only rows where count_na[2] is 15
subset15 <- na_count[na_count$count_na == 15, ]

# print the values of count_na[1] in the subset
print(subset15)



# Which countries are missing 9 values?
# subset na_count to include only rows where count_na[2] is 15
subset9 <- na_count[na_count$count_na == 9, ]

# print the values of count_na[1] in the subset
print(subset9, n=34)


# Exploring the subset9 using code like
# print(filter(df_by_country, country == "Cook Islands"))
# revealed that only three years are available for these countries, and all the suicide data are missing.
# This is because the countries were only in the gdb dataset, not in the WB dataset.

# remove countries with no data on suicide rates

# create a vector of values to exclude
exclude_vec <- as.character(c(subset15[[1]], subset9[[1]]))
exclude_vec
# subset the data frame to exclude rows where the value in the column is in the exclude_vec
wb_gbd_wide <- subset(wb_gbd_wide, !country %in% exclude_vec)

test_countries <- as.list(unique(wb_gbd_wide[c("country")]))
length(test_countries[[1]])
# we have 183 countries





### MISSING VALUES IN TIME SERIES


# Visualising:

library(tidyverse)
library(naniar)
library(ggplot2)

# # plot missing data by country and sex --> too many plots
# wb_gbd_wide %>%
#   group_by(country, sex) %>%
#   ggplot(aes(x = year, y = sui)) +
#   geom_line(aes(color = is.na(sui))) +
#   facet_grid(cols = vars(country), rows = vars(sex)) +
#   scale_color_manual(name = "Missing data", values = c("TRUE" = "red", "FALSE" = "black")) +
#   labs(x = "Year", y = "Suicide rate") +
#   theme_bw()




library(visdat)
library(dplyr)

# getting a plot with a pattern of missing data.
# Note: I used this plot to go back upstream in my code and improve the result by removing countries with missing data on the suicide variable.


# Group the data by country
df_by_country <- wb_gbd_wide %>%
  group_by(country, sex) %>%
  arrange(year)

# Create a heatmap of missing data using vis_miss()
vis_miss(df_by_country)


# A Table showing the number of missings per column by country, only for those with missing data.
library(dplyr)
# Group by country and summarize missing values in each column
df_miss <- df_by_country %>% 
  group_by(country) %>% 
  summarize_at(vars(-group_cols()), ~sum(is.na(.)))

# Filter only those with missing values
df_miss_filtered <- df_miss %>% 
  filter(rowSums(df_miss[, -1]) > 0)

# View the result
print(df_miss_filtered, n=200)



library(pheatmap)

# Transpose the data frame so that variables are columns and countries are rows
df_miss_transposed <- t(df_miss_filtered[, -1])

# Create a heatmap with pheatmap
pdf("../outputs/heatmap_missings.pdf")

pheatmap(df_miss_transposed, 
         color = colorRampPalette(c("yellow", "purple"))(100), 
         cluster_cols = TRUE)
dev.off()


# The heatmap shows the pattern of missing values across the variables and countries. Each row in the heatmap corresponds to a variable, and each column corresponds to a country. The cells in the heatmap are colored according to the proportion of missing values for that variable in that country, with white cells indicating no missing values, and blue cells indicating high proportions of missing values.
# 
# Interpreting the heatmap involves looking for patterns in the missing data across the different variables and countries. Here are some general guidelines:
#   
#   Look for variables that have a high proportion of missing values across many countries. These variables may be difficult to work with, as they may limit the scope of analysis or introduce bias.
#   Look for countries that have a high proportion of missing values across many variables. These countries may be underrepresented in the analysis or may require imputation methods to handle the missing data.
#   Look for patterns of missingness that may be related to other variables or factors. For example, if a country has a high proportion of missing values for income and education variables, it may be an indication of socioeconomic disparities or differences in data collection methods.
# In general, interpreting missing data can be complex and may require additional information about the variables and countries in question. The heatmap can provide a useful visual summary of the missing data patterns, but it should be used in conjunction with other analyses and considerations.


# Replace where it's missing in the series.


# https://stackoverflow.com/questions/50648800/ggplot-plotting-timeseries-data-with-missing-values
# na_locf() from package zoo

library(dplyr)
library(imputeTS)
# Impute missing values using median by group (country, sex). There may be a more efficient way of doing this
df_imputed <- df_by_country %>%
  mutate(gdp_pc = median(gdp_pc, na.rm = TRUE),
         edu = median(edu, na.rm = TRUE),
         sui = median(sui, na.rm = TRUE),
         unem_y = median(unem_y, na.rm = TRUE),
         unem_t = median(unem_t, na.rm = TRUE),
         pop_t = median(pop_t, na.rm = TRUE),
         alc = median(alc, na.rm = TRUE),
         drug = median(drug, na.rm = TRUE),
         depr = median(depr, na.rm = TRUE),
         sh = median(sh, na.rm = TRUE))

summary(df_by_country)
summary(df_imputed)




# This approach isn't all that successful --> imputing median by region


df_by_region <- wb_gbd_wide %>%
  group_by(region, sex) %>%
  arrange(year)

# Repeating imputation by region Obviously not the correct thing to do!

df_imputed2 <- df_by_region %>%
  mutate(gdp_pc = median(gdp_pc, na.rm = TRUE),
         edu = median(edu, na.rm = TRUE),
         sui = median(sui, na.rm = TRUE),
         unem_y = median(unem_y, na.rm = TRUE),
         unem_t = median(unem_t, na.rm = TRUE),
         pop_t = median(pop_t, na.rm = TRUE),
         alc = median(alc, na.rm = TRUE),
         drug = median(drug, na.rm = TRUE),
         depr = median(depr, na.rm = TRUE),
         sh = median(sh, na.rm = TRUE))

summary(df_by_country)
summary(df_imputed2)


# And finally by continent


df_by_continent <- wb_gbd_wide %>%
  group_by(continent, sex) %>%
  arrange(year)

# Repeating imputation by region Obviously not the correct thing to do!

df_imputed3 <- df_by_continent %>%
  mutate(gdp_pc = median(gdp_pc, na.rm = TRUE),
         edu = median(edu, na.rm = TRUE),
         sui = median(sui, na.rm = TRUE),
         unem_y = median(unem_y, na.rm = TRUE),
         unem_t = median(unem_t, na.rm = TRUE),
         pop_t = median(pop_t, na.rm = TRUE),
         alc = median(alc, na.rm = TRUE),
         drug = median(drug, na.rm = TRUE),
         depr = median(depr, na.rm = TRUE),
         sh = median(sh, na.rm = TRUE))

summary(df_by_country)
summary(df_imputed3)







# This is where I stopped -------------------------------------------------


### df_imputed3 now has no missing values!!!! Phew.

# clean up global environment:
#   # Remove multiple objects from the global environment
#   rm(my_data, other_data, unused_vector)



  
         
#https://stackoverflow.com/questions/37975872/how-to-use-mutate-on-list





# Not needed for this project but took me a while to figure out this loop.
# # original column names
# varnames <- as.list(colnames(wb_gbd_wide))
# 
# #varnames 
# 
# varnames.y <- list()
# 
# for (i in varnames) {
#   y <- paste0(i,".y")
#   #print(i)
#   print(y)
#   varnames.y <- append(varnames.y, y)
# }



# This is the wrong thing to do!
# # Removing redundant columns
# wb_gbd_wide <- wb_gbd_wide.tmp2[-c(5:16)]
# 
# names(wb_gbd_wide) <- sapply(strsplit(names(wb_gbd_wide), ".y"), `[[`, 1)


