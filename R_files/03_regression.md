

```r
# Fit regression(s)

# Regression diagnostics (esp. visual)


#https://www.datanovia.com/en/blog/how-to-plot-a-smooth-line-using-ggplot2/


summary(suicide_final)
```

```
##     iso3c             country               year          sex                gdp_pc            edu              sui             unem_y           unem_t           pop_t                alc         
##  Length:13566       Length:13566       Min.   :2000   Length:13566       Min.   :   261   Min.   : 0.000   Min.   :  0.00   Min.   : 0.194   Min.   : 0.050   Min.   :1.044e+04   Min.   :  52.31  
##  Class1:labelled    Class1:labelled    1st Qu.:2005   Class1:labelled    1st Qu.:  2128   1st Qu.: 8.000   1st Qu.:  4.30   1st Qu.: 8.880   1st Qu.: 4.054   1st Qu.:1.436e+06   1st Qu.: 287.63  
##  Class2:character   Class2:character   Median :2011   Class2:character   Median :  6019   Median :10.000   Median :  7.30   Median :14.293   Median : 6.061   Median :8.053e+06   Median : 576.05  
##  Mode  :character   Mode  :character   Mean   :2011   Mode  :character   Mean   : 14128   Mean   : 9.704   Mean   : 10.15   Mean   :17.320   Mean   : 7.747   Mean   :3.794e+07   Mean   : 744.55  
##                                        3rd Qu.:2016                      3rd Qu.: 15600   3rd Qu.:11.000   3rd Qu.: 13.25   3rd Qu.:23.000   3rd Qu.: 9.562   3rd Qu.:2.720e+07   3rd Qu.:1087.74  
##                                        Max.   :2021                      Max.   :204190   Max.   :17.000   Max.   :147.80   Max.   :80.762   Max.   :37.901   Max.   :1.412e+09   Max.   :3678.89  
##       drug             depr             sh           continent            region              year1                deaths         
##  Min.   : 68.72   Min.   : 1034   Min.   :  6.791   Length:13566       Length:13566       Min.   :2000-01-01   Min.   :     0.00  
##  1st Qu.:144.79   1st Qu.: 2711   1st Qu.: 23.832   Class1:labelled    Class1:labelled    1st Qu.:2005-01-01   1st Qu.:    63.64  
##  Median :194.73   Median : 3642   Median : 39.908   Class2:character   Class2:character   Median :2011-01-01   Median :   354.89  
##  Mean   :207.38   Mean   : 3869   Mean   : 60.656   Mode  :character   Mode  :character   Mean   :2010-07-10   Mean   :  2453.53  
##  3rd Qu.:248.73   3rd Qu.: 4747   3rd Qu.: 84.894                                         3rd Qu.:2016-01-01   3rd Qu.:  1163.88  
##  Max.   :618.78   Max.   :11304   Max.   :883.830                                         Max.   :2021-01-01   Max.   :182251.67
```

```r
library(fixest)
```

```
## 
## Attaching package: 'fixest'
```

```
## The following object is masked from 'package:terra':
## 
##     panel
```

```r
library(modelsummary)
library(stats)

library(sjPlot)
```

```
## Learn more about sjPlot with 'browseVignettes("sjPlot")'.
```

```r
library(sjlabelled)
```

```
## 
## Attaching package: 'sjlabelled'
```

```
## The following object is masked from 'package:expss':
## 
##     read_spss
```

```
## The following object is masked from 'package:forcats':
## 
##     as_factor
```

```
## The following object is masked from 'package:ggplot2':
## 
##     as_label
```

```
## The following object is masked from 'package:dplyr':
## 
##     as_label
```

```r
library(sjmisc)
```

```
## 
## Attaching package: 'sjmisc'
```

```
## The following objects are masked from 'package:BBmisc':
## 
##     %nin%, seq_col, seq_row
```

```
## The following object is masked from 'package:raster':
## 
##     trim
```

```
## The following object is masked from 'package:terra':
## 
##     trim
```

```
## The following object is masked from 'package:naniar':
## 
##     all_na
```

```
## The following objects are masked from 'package:expss':
## 
##     add_columns, add_rows, rec
```

```
## The following object is masked from 'package:maditr':
## 
##     to_long
```

```
## The following object is masked from 'package:purrr':
## 
##     is_empty
```

```
## The following object is masked from 'package:tibble':
## 
##     add_case
```

```
## The following object is masked from 'package:tidyr':
## 
##     replace_na
```

```r
library(ggplot2)
```

Does the suicide rate increase over time for males or females?


```r
# Simplest model
reg1 <- suicide_final %>% filter(sex != "Both") %>% feols(sui ~ sex + year + sex*year)

# Fixed effect by country
reg2 <- suicide_final %>% filter(sex != "Both") %>% feols(sui ~ sex + year + sex*year | country)

# Including economic data
reg3 <- suicide_final %>% filter(sex != "Both") %>% feols(sui ~ sex + year + sex*year + pop_t + 
                                                            edu + gdp_pc + unem_y + unem_t | country)
# Including mental health data
reg4 <- suicide_final %>% filter(sex != "Both") %>% feols(sui ~ sex + year + sex*year + pop_t 
                                                          + edu + gdp_pc + unem_y + unem_t+ 
                                                            alc + drug + depr + sh | country)
# 4 plus fixed effect for year
reg5 <- suicide_final %>% filter(sex != "Both") %>% feols(sui ~ sex + pop_t 
                                                          + edu + gdp_pc + unem_y + unem_t+ 
                                                            alc + drug + depr + sh | country + year,
                                                          cluster = ~ country)
# 5 minus vars with zero coefficient
reg6 <- suicide_final %>% filter(sex != "Both") %>% feols(sui ~ sex + 
                                                          edu + unem_y + unem_t+ 
                                                            alc + drug + depr + sh | country + year,
                                                          cluster = ~ country)
```

Overview of models


```r
models <- list(reg1, reg2, reg3, reg4, reg5, reg6)

coefs = c("sexMale" = "Male", "year" = "Year", "sexMale year" = "Male * Year", "pop_t" = "Total population", "edu" = "Years compulsory education", "gdp_pc" = "GDP per capita", "unem_y" = "Youth unemployment", "unem_t" = "Total unemployment", "alc" = "Alcohol misuse", "drug" = "Drug misuse", "sh" = "Self harm")

model_summary <- modelsummary(models,
                              coef_omit = "Intercept",
                              coef_map = coefs,
                              stars = TRUE)
```

Showing the coefficients and standard errors for the 5 models


```r
model_plot <- modelplot(models,
                        coef_map = c("sexMale" = "Male")) +
  guides(color = guide_legend(reverse = TRUE))

model_summary
```

<table style="NAborder-bottom: 0; width: auto !important; margin-left: auto; margin-right: auto;" class="table">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:center;">  (1) </th>
   <th style="text-align:center;">   (2) </th>
   <th style="text-align:center;">   (3) </th>
   <th style="text-align:center;">   (4) </th>
   <th style="text-align:center;">   (5) </th>
   <th style="text-align:center;">   (6) </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Male </td>
   <td style="text-align:center;"> −1281.432*** </td>
   <td style="text-align:center;"> −1281.432*** </td>
   <td style="text-align:center;"> −1286.699*** </td>
   <td style="text-align:center;"> −1365.098*** </td>
   <td style="text-align:center;"> 4.250*** </td>
   <td style="text-align:center;"> 4.260*** </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:center;"> (64.095) </td>
   <td style="text-align:center;"> (101.518) </td>
   <td style="text-align:center;"> (95.339) </td>
   <td style="text-align:center;"> (107.041) </td>
   <td style="text-align:center;"> (0.539) </td>
   <td style="text-align:center;"> (0.539) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Year </td>
   <td style="text-align:center;"> −0.316*** </td>
   <td style="text-align:center;"> −0.321*** </td>
   <td style="text-align:center;"> −0.323*** </td>
   <td style="text-align:center;"> −0.325*** </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:center;"> (0.023) </td>
   <td style="text-align:center;"> (0.025) </td>
   <td style="text-align:center;"> (0.024) </td>
   <td style="text-align:center;"> (0.024) </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Total population </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;"> 0.000 </td>
   <td style="text-align:center;"> 0.000 </td>
   <td style="text-align:center;"> 0.000+ </td>
   <td style="text-align:center;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;"> (0.000) </td>
   <td style="text-align:center;"> (0.000) </td>
   <td style="text-align:center;"> (0.000) </td>
   <td style="text-align:center;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Years compulsory education </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;"> −0.207* </td>
   <td style="text-align:center;"> −0.232* </td>
   <td style="text-align:center;"> −0.044 </td>
   <td style="text-align:center;"> −0.046 </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;"> (0.081) </td>
   <td style="text-align:center;"> (0.093) </td>
   <td style="text-align:center;"> (0.099) </td>
   <td style="text-align:center;"> (0.100) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> GDP per capita </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;"> 0.000+ </td>
   <td style="text-align:center;"> 0.000+ </td>
   <td style="text-align:center;"> 0.000+ </td>
   <td style="text-align:center;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;"> (0.000) </td>
   <td style="text-align:center;"> (0.000) </td>
   <td style="text-align:center;"> (0.000) </td>
   <td style="text-align:center;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Youth unemployment </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;"> −0.260 </td>
   <td style="text-align:center;"> −0.246 </td>
   <td style="text-align:center;"> −0.297 </td>
   <td style="text-align:center;"> −0.292 </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;"> (0.176) </td>
   <td style="text-align:center;"> (0.169) </td>
   <td style="text-align:center;"> (0.182) </td>
   <td style="text-align:center;"> (0.181) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Total unemployment </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;"> 0.514* </td>
   <td style="text-align:center;"> 0.446+ </td>
   <td style="text-align:center;"> 0.272 </td>
   <td style="text-align:center;"> 0.274 </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;"> (0.259) </td>
   <td style="text-align:center;"> (0.243) </td>
   <td style="text-align:center;"> (0.255) </td>
   <td style="text-align:center;"> (0.255) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Alcohol misuse </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;"> 0.001** </td>
   <td style="text-align:center;"> 0.001* </td>
   <td style="text-align:center;"> 0.001* </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;"> (0.000) </td>
   <td style="text-align:center;"> (0.000) </td>
   <td style="text-align:center;"> (0.000) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Drug misuse </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;"> 0.005 </td>
   <td style="text-align:center;"> −0.010+ </td>
   <td style="text-align:center;"> −0.010+ </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;"> (0.006) </td>
   <td style="text-align:center;"> (0.006) </td>
   <td style="text-align:center;"> (0.006) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Self harm </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;"> 0.040** </td>
   <td style="text-align:center;"> 0.048** </td>
   <td style="text-align:center;"> 0.048** </td>
  </tr>
  <tr>
   <td style="text-align:left;box-shadow: 0px 1px">  </td>
   <td style="text-align:center;box-shadow: 0px 1px">  </td>
   <td style="text-align:center;box-shadow: 0px 1px">  </td>
   <td style="text-align:center;box-shadow: 0px 1px">  </td>
   <td style="text-align:center;box-shadow: 0px 1px"> (0.012) </td>
   <td style="text-align:center;box-shadow: 0px 1px"> (0.015) </td>
   <td style="text-align:center;box-shadow: 0px 1px"> (0.015) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Num.Obs. </td>
   <td style="text-align:center;"> 9044 </td>
   <td style="text-align:center;"> 9044 </td>
   <td style="text-align:center;"> 9044 </td>
   <td style="text-align:center;"> 9044 </td>
   <td style="text-align:center;"> 9044 </td>
   <td style="text-align:center;"> 9044 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> R2 </td>
   <td style="text-align:center;"> 0.088 </td>
   <td style="text-align:center;"> 0.837 </td>
   <td style="text-align:center;"> 0.839 </td>
   <td style="text-align:center;"> 0.850 </td>
   <td style="text-align:center;"> 0.810 </td>
   <td style="text-align:center;"> 0.810 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> R2 Adj. </td>
   <td style="text-align:center;"> 0.087 </td>
   <td style="text-align:center;"> 0.833 </td>
   <td style="text-align:center;"> 0.835 </td>
   <td style="text-align:center;"> 0.846 </td>
   <td style="text-align:center;"> 0.805 </td>
   <td style="text-align:center;"> 0.805 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> R2 Within </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;"> 0.350 </td>
   <td style="text-align:center;"> 0.359 </td>
   <td style="text-align:center;"> 0.400 </td>
   <td style="text-align:center;"> 0.241 </td>
   <td style="text-align:center;"> 0.240 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> R2 Within Adj. </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;"> 0.350 </td>
   <td style="text-align:center;"> 0.358 </td>
   <td style="text-align:center;"> 0.399 </td>
   <td style="text-align:center;"> 0.240 </td>
   <td style="text-align:center;"> 0.240 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> AIC </td>
   <td style="text-align:center;"> 66546.8 </td>
   <td style="text-align:center;"> 51389.7 </td>
   <td style="text-align:center;"> 51272.6 </td>
   <td style="text-align:center;"> 50683.6 </td>
   <td style="text-align:center;"> 52818.7 </td>
   <td style="text-align:center;"> 52821.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> BIC </td>
   <td style="text-align:center;"> 66575.3 </td>
   <td style="text-align:center;"> 52904.1 </td>
   <td style="text-align:center;"> 52822.6 </td>
   <td style="text-align:center;"> 52262.0 </td>
   <td style="text-align:center;"> 54532.2 </td>
   <td style="text-align:center;"> 54521.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> RMSE </td>
   <td style="text-align:center;"> 9.58 </td>
   <td style="text-align:center;"> 4.05 </td>
   <td style="text-align:center;"> 4.02 </td>
   <td style="text-align:center;"> 3.89 </td>
   <td style="text-align:center;"> 4.37 </td>
   <td style="text-align:center;"> 4.37 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Std.Errors </td>
   <td style="text-align:center;"> IID </td>
   <td style="text-align:center;"> by: country </td>
   <td style="text-align:center;"> by: country </td>
   <td style="text-align:center;"> by: country </td>
   <td style="text-align:center;"> by: country </td>
   <td style="text-align:center;"> by: country </td>
  </tr>
  <tr>
   <td style="text-align:left;"> FE: country </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
  </tr>
  <tr>
   <td style="text-align:left;"> FE: year </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> X </td>
  </tr>
</tbody>
<tfoot><tr><td style="padding: 0; " colspan="100%">
<sup></sup> + p &lt; 0.1, * p &lt; 0.05, ** p &lt; 0.01, *** p &lt; 0.001</td></tr></tfoot>
</table>







```r
model_plot
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)


