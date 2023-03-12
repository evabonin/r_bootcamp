

###############################################################################
# TRY AND FIX THIS BIT
###############################################################################

# 
# # # A Table showing the number of missings per column by country, only for those with missing data.
# library(dplyr)
# 
# # This was working yesterday...
# # Group by country and summarize missing values in each column
# df_miss <- df_by_country %>%
#   group_by(country) %>%
#   summarize_at(vars(-group_cols()), ~sum(is.na(.)))
# 
# # Filter only those with missing values
# df_miss_filtered <- df_miss %>%
#   filter(rowSums(df_miss[, -1]) > 0)
# 
# # View the result
# print(df_miss_filtered, n=250)
# 
# 
# library(pheatmap)
# 
# # Transpose the data frame so that variables are columns and countries are rows
# df_miss_transposed <- t(df_miss_filtered[, -1])
# 
# # Create a heatmap with pheatmap
# pdf("../outputs/heatmap_missings.pdf")
# 
# pheatmap(df_miss_transposed, 
#          color = colorRampPalette(c("yellow", "purple"))(100), 
#          cluster_cols = TRUE)
# dev.off()


# The heatmap shows the pattern of missing values across the variables and countries. Each row in the heatmap corresponds to a variable, and each column corresponds to a country. The cells in the heatmap are colored according to the proportion of missing values for that variable in that country, with white cells indicating no missing values, and blue cells indicating high proportions of missing values.
# 
# Interpreting the heatmap involves looking for patterns in the missing data across the different variables and countries. Here are some general guidelines:
#   
#   Look for variables that have a high proportion of missing values across many countries. These variables may be difficult to work with, as they may limit the scope of analysis or introduce bias.
#   Look for countries that have a high proportion of missing values across many variables. These countries may be underrepresented in the analysis or may require imputation methods to handle the missing data.
#   Look for patterns of missingness that may be related to other variables or factors. For example, if a country has a high proportion of missing values for income and education variables, it may be an indication of socioeconomic disparities or differences in data collection methods.
# In general, interpreting missing data can be complex and may require additional information about the variables and countries in question. The heatmap can provide a useful visual summary of the missing data patterns, but it should be used in conjunction with other analyses and considerations.


###############################################################################
###############################################################################

#' Replace where it's missing in the series.


# https://stackoverflow.com/questions/50648800/ggplot-plotting-timeseries-data-with-missing-values
# na_locf() from package zoo




