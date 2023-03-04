wb_gbd_wide.tmp <- data.frame(country = rep("GB", 30), 
                              year = 2000:2029)

wb_gbd_wide <- cbind(rbind(wb_gbd_wide.tmp,
                           wb_gbd_wide.tmp), 
                     "gender" = rep(c("both", "male"), each = 30), 
                     "value" = c(rnorm(30), rep(NA, 30)))

library(dplyr)

## Prepare second data set
wb_gbd_wide.both <- wb_gbd_wide %>% 
  filter(gender == "both") %>% 
  select(!gender)

## Join the two data sets because you have to make sure that contry and year
## are correctly merged. 
wb_gbd_wide.tmp2 <- 
  left_join(x = wb_gbd_wide, y = wb_gbd_wide.both, 
            by = c("country", "year"),
            suffix = c("", ".y"))


## There are multiple ways to do so... Maybe you do not want to overwrite all 
## values, but only the ones for male. 
## Or you already do this in the join above...
wb_gbd_wide <- wb_gbd_wide.tmp2 %>% 
  mutate(value = value.y) %>% 
  select(!value.y)
