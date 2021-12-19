

# Data Grapling

## EDA
1.Take a look at data
2.Visualize Data
3.Count basic statistics


One quick technique for jump-starting EDA is to examine all of the pairwise scatterplots in your data. This can be achieved using the pairs() function. Look for variables in the nyc data set that are strongly correlated, as those relationships will help us check for multicollinearity later on. 

```r
df_it_restaurants <- read.csv('https://assets.datacamp.com/production/repositories/845/datasets/639a7a3f9020edb51bcbc4bfdb7b71cbd8b9a70e/nyc.csv')
pairs(df_it_restaurants %>% select(-Restaurant))
```

<img src="1_Data_Grapling_files/figure-html/unnamed-chunk-1-1.png" width="672" />

## Missing Data 

NA = Not Available
NaN = Not a Number


```r
library('datasets')

df_airquality <- airquality

NA|TRUE
```

```
## [1] TRUE
```

```r
NA|FALSE
```

```
## [1] NA
```

```r
NA+NaN
```

```
## [1] NA
```

```r
NaN+NA
```

```
## [1] NaN
```

```r
library(naniar)
```
