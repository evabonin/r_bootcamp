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




world_bank <- wb_data(country = "countries_only",
                      indicator = c("SP.POP.TOTL", "SH.STA.SUIC.P5" , "SH.STA.SUIC.FE.P5" , "SH.STA.SUIC.MA.P5" , "SE.COM.DURS" , "NY.GDP.MKTP.KD" , "NY.GDP.PCAP.KD" , "SL.UEM.1524.ZS" , "SL.UEM.1524.MA.ZS" , "SL.UEM.1524.FE.ZS" , "SL.UEM.TOTL.ZS" , "SL.UEM.TOTL.MA.ZS" , "SL.UEM.TOTL.FE.ZS"),
                      mrv = 5,
                      gapfill = TRUE) %>% 
  filter(date >= 2000)


varnames <- c("ISO2C", "ISO3C", "Country", "Year", "GDP (constant 2015 US$)", "GDP per capita (constant 2015 US$)", "Compulsory education, duration (years)" , 
              "Suicide mortality rate (per 100,000 population) female", "Suicide mortality rate (per 100,000 population) male", "Suicide mortality rate (per 100,000 population)", 
              "Unemployment, youth total (% of total labor force ages 15-24) (modeled ILO estimate) female" ,   "Unemployment, youth total (% of total labor force ages 15-24) (modeled ILO estimate) male" , 
              "Unemployment, youth total (% of total labor force ages 15-24) (modeled ILO estimate)" ,
              "Unemployment, total (% of total labor force) (modeled ILO estimate) female", "Unemployment, total (% of total labor force) (modeled ILO estimate) male",
              "Unemployment, total (% of total labor force) (modeled ILO estimate)", "Total population")


colnames(world_bank) <- varnames


#world_bank_long <- melt(world_bank, id=c("country","iso3c", "date"), variable_name = "indicator")


wb_long <- world_bank %>%
  pivot_longer(cols = !c(Country, ISO3C, ISO2C, Year),
               names_to = "Indicator",
               values_to = "Value"
               )
wb_long[wb_long == 'NULL'] <- NA

# Creating gender column in WB data

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

gbd <- read.csv("../data/gbd/gbd_clean.csv", header = TRUE, sep = ",", na.strings = "NA")



## Dropping columns we don't need

gbd <- gbd[, -c(1, 2, 3, 5, 7, 8, 9, 11, 12, 15, 16)]


# insert column with country codes
# converting country names to world bank destination coding scheme using countrycode library. target var is ISO3C

gbd <- gbd %>%
  mutate(ISO3C = countrycode(location_name,"country.name", "wb"))



#check to see if there are any cases missing in ISO3C

filter(gbd, is.na(ISO3C))

# Cook Islands self harm
# Tokelau depressive disorders
# Niue Depressive disorders

# There are a few missing, but will deal with this later


# Dropping rows relating to sexual voilence because thta appears to be zero throughout.

gbd <- gbd[gbd$cause_name != "Sexual violence", ] 



# Renaming columns


gbd_oldnames <- colnames(gbd)
gbd_oldnames
gbd_newnames <- c("Country", "Sex", "Indicator", "Year", "Value", "ISO3C" )
colnames(gbd) <- gbd_newnames


# MERGING DATASETS
wb_gbd_long <- rbind(wb_long, gbd)
wb_gbd_long[wb_gbd_long == 'NULL'] <- NA


# Creating wide version of wb_gbd

wb_gbd_wide <- wb_gbd_long %>%
  pivot_wider(names_from = "Indicator",
               values_from = "Value"
  )

# wb_gbd_long %>%
#   dplyr::group_by(ISO3C, Country, Year, Sex, Indicator) %>%
#   dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
#   dplyr::filter(n > 1L) 


# Checking country names for combined dataset as some variance in spelling was found:

countries_wb_gbd <- as.list(unique(wb_gbd_wide[c("Country")]))
countries_wb_gbd <- lapply(countries_wb_gbd, sort, decreasing = FALSE)
countries_wb_gbd <- as.data.frame(countries_wb_gbd)

# This list will be later to filter the data for imputation
countries_wb_gbd

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


# Unique values for Year variable --> will be used for filtering.

years <- as.list(unique(wb_gbd_wide[c("Year")]))
years <- lapply(years, sort, decreasing = FALSE)

# Imputing missing values

# Considerations for imputation:
# - GDP, GDP / capita, compulsory education, total population: same value for male / female as overall. Can use na_mean from imputeTS package.
# - Then, for all vars: Missing years --> interpolation

both <- filter(wb_gbd_wide, wb_gbd_wide$Country == "Aruba" & wb_gbd_wide$Year == "2017" & wb_gbd_wide$Sex == "Both")
test <- both$`GDP per capita (constant 2015 US$)`
test


# This finally works.But need to save it to the dataset somehow.
filter(wb_gbd_wide, wb_gbd_wide$Country == "Aruba" & wb_gbd_wide$Year == "2017" & wb_gbd_wide$Sex != "Both") %>% replace_na(list(`GDP per capita (constant 2015 US$)`= test))


# Another attempt, there seems to be a typo around line 230 but I don't know what it is.

wb_gbd_wide %>%
  group_by(wb_gbd_wide$Country) %>%
  mutate(
    wb_gbd_wide$`GDP (constant 2015 US$)` = impute.mean(wb_gbd_wide$`GDP (constant 2015 US$)`)
  )





# Now trying with loop


for (x in countries_wb_gbd) {
  #print(x)
  x <- filter(wb_gbd_wide, wb_gbd_wide$Country == x & wb_gbd_wide$Year == "2017" & wb_gbd_wide$Sex != "Both")
}





wb_gbd_wide %>% group_by(wb_gbd_wide$Country) %>% group_by(wb_gbd_wide$Year) %>% mutate()



test2 <-filter(wb_gbd_wide, wb_gbd_wide$Country == "Germany" & wb_gbd_wide$Year == "2021")
test2$GDP

wb_gbd_wide











