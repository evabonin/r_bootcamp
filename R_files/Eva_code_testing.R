

## Loading World Bank data. Setting ".." to NA = missing data

world_bank <- read.table("../data/world bank indicators/wb_indicators.csv", header = TRUE, sep = ",", na.strings = "..")
indicators <- unique(world_bank[c("Series.Name", "Series.Code")])

library("writexl")
write_xlsx(indicators,"indicators.xlsx")


# Changing column names using regular expressions
wb_oldnames <- colnames(world_bank)
wb_newnames <- gsub(old_names, pattern = "(X)([0-9]+)(.+)", replacement = "\\2")
#wb_newnames

colnames(world_bank) <- wb_newnames

# Reshaping to long in "year" variable

library(reshape)
world_bank_long <- melt(world_bank, id=c("Country.Name","Country.Code", "Series.Name", "Series.Code"), variable_name = "Year")

write.csv(world_bank_long, "../data/world bank indicators/world_bank_long.csv", row.names=TRUE)



# Creating a list of country codes to use later on merged dataset. Working fine
library(countrycode)



country_codes <- unique(world_bank_long[c("Country.Name", "Country.Code")])
country_codes





# Creating gender column in WB data

library(dplyr)
library(stringr)



world_bank_long <- world_bank_long %>%
  mutate(Gender = case_when(
    str_detect(Series.Name, "female") ~ "Female",
    str_detect(Series.Name, "male") ~ "Male"),
    Gender = ifelse(is.na(Gender), "Both", Gender))
    



# Renaming indicators so they no longer contain male and female







# Loading GBD data

gbd <- read.table("../data/gbd/gbd.csv", header = TRUE, sep = ",", na.strings = "..")

## Dropping columns we don't need

gbd <- gbd[, -c(1, 2, 3, 5, 7, 8, 9, 11, 12, 15, 16)]


# insert column with country codes

gbd <- gbd %>%
  mutate(Country.Codes = filter(location_name == country_codes$Country.Name),
         .after="location_name")



gbd_oldnames <- colnames(gbd)
gbd_newnames <- 

colnames(gbd) <- gbd_newnames






























## Testing: making a world map
# https://cran.r-project.org/web/packages/rworldmap/vignettes/rworldmap.pdf

library(rworldmap)

# Joining the data to a country map on Country.Code variable
sPDF <- joinCountryData2Map(world_bank_long, joinCode = "ISO3", nameJoinColumn = "Country.Code", verbose = TRUE)
# This doesn't map regions (only countries), which makes sense

par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
mapCountryData( sPDF, nameColumnToPlot="value")

# This doesn't make a lot of sense yet because "value" represents a lot of different indicators and it looks like it just adds them all up.
# Trying again just using suicide rates (overall, by country)

#wb_suicides <- world_bank_long



## One map for suicide rates for each year.
# Suggestion: ggplot with for loop and print. Instead, using rworldmap as above.

library(dplyr)
library(ggplot2)

for (i in 2000:2019) { 
  map <- paste0("graphs/suicide_rates_map",i, ".png")
  #print(map)
  png(map,width=800,height=800,units="px")
  df <- world_bank_long %>% 
    filter(Year == i) %>% 
    filter(Series.Code == "SH.STA.SUIC.P5")
  sPDF <- joinCountryData2Map(df, joinCode = "ISO3", nameJoinColumn = "Country.Code", verbose = FALSE)
  par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
  mapCountryData(sPDF, nameColumnToPlot="value")
  dev.off()
}
  



