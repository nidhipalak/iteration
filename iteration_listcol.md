Iteration and List Columns
================
Nidhi Patel
11/17/2020

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.3     ✓ dplyr   1.0.2
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(rvest)
```

    ## Loading required package: xml2

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     pluck

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
knitr::opts_chunk$set(
  fig.width = 6,
  fig.height = 6,
  out.width = "90%")

theme_set(theme_minimal() + theme(legend.position = "bottom"))

options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_color_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

## Lists

You can put anything in a list

``` r
l = list(
  vec_num = 5:8,
  vec_log = c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE),
  mat = matrix(1:8, nrow = 2, ncol = 4),
  summary = summary(rnorm(100))
)
#there is no way this would go into a dataframe (number of arguments do not match) but it can go into a list
```

How to access parts of list.

``` r
l$vec_num
```

    ## [1] 5 6 7 8

``` r
l[[1]]
```

    ## [1] 5 6 7 8

``` r
l[["vec_num"]]
```

    ## [1] 5 6 7 8

## `for` loop

Create a new list

``` r
list_norm = 
  list(
    a = rnorm(20, mean = 3, sd = 1),
    b = rnorm(30, mean = 0, sd = 5),
    c = rnorm(40, mean = 7, sd = .6),
    d = rnorm(50, mean = -3, sd = 1)
  )
```

``` r
list_norm
```

    ## $a
    ##  [1] 3.6640013 3.8394707 5.0302817 4.3031567 3.1240245 2.4951346 3.6455500
    ##  [8] 1.4496586 4.3472226 2.5129448 2.6227573 2.4577284 3.8664397 3.6117810
    ## [15] 3.8701177 1.4513888 1.7286617 3.7641359 2.5299405 0.5280561
    ## 
    ## $b
    ##  [1]  -0.01368748  -1.85324681  10.95292340  -4.87212862  -9.37810912
    ##  [6]   2.41886739   7.90229183   1.82222685   0.81277929  -9.13053634
    ## [11]   3.88831638   3.72249851  -1.75942473   6.37499915   9.14673566
    ## [16]  -9.27788815   5.90053417   8.37755269   3.02435059   3.46101885
    ## [21]   9.37248659  -9.48593735  -0.82686026   5.09782153  -9.62205517
    ## [26]   8.02479974   2.72814783  -8.92176918 -12.60891755  -3.90084881
    ## 
    ## $c
    ##  [1] 6.823863 7.448474 7.570824 5.841022 7.017768 7.455869 5.217131 6.802839
    ##  [9] 6.543813 7.478909 7.125215 7.179918 6.833974 6.857625 7.402836 5.150440
    ## [17] 6.005091 5.676838 6.385484 8.430348 6.652222 7.095361 6.640248 7.238926
    ## [25] 6.876053 6.929894 6.535965 6.898350 6.575252 7.206467 6.803136 6.934304
    ## [33] 6.166704 6.994951 6.433262 7.290498 7.137239 7.619602 6.781857 6.532246
    ## 
    ## $d
    ##  [1] -2.676720 -3.524387 -1.875402 -3.070962 -2.280767 -2.787047 -2.419002
    ##  [8] -4.314094 -2.597613 -2.590990 -1.829274 -2.640614 -2.114904 -2.782131
    ## [15] -3.734329 -4.588318 -2.805544 -4.376762 -4.332943 -2.588895 -3.370863
    ## [22] -1.999950 -2.781997 -3.565840 -3.357355 -2.695253 -3.269262 -3.078175
    ## [29] -1.578155 -1.023197 -2.452556 -3.381013 -3.007679 -2.586079 -2.822294
    ## [36] -4.353032 -2.108793 -3.227078 -2.910682 -1.675322 -2.522573 -4.623132
    ## [43] -3.137879 -4.243848 -3.735748 -2.049175 -4.394520 -2.489312 -3.730363
    ## [50] -3.705087

Pause and get my old fn

``` r
mean_sd = function(x) {
 if (!is.numeric(x)) {
   stop("Input must be numeric")
 }
  if (length(x) < 3) {
    stop("Input must have at least three numbers")
  }
 mean_x = mean(x)
 sd_x = sd(x)
 
 tibble(
   mean = mean_x,
   sd = sd_x
 )
}
```

I can apply that fn to each list element

``` r
mean_sd(list_norm[["a"]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.04  1.15

``` r
mean_sd(list_norm[["b"]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.379  6.89

``` r
mean_sd(list_norm[["c"]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.81 0.637

``` r
mean_sd(list_norm[["d"]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.00 0.855

Let’s use a for loop:

``` r
output = vector("list", length = 4)
#above is empty MAKE IT FIRST

for (i in 1:4) {
output[[i]] = mean_sd(list_norm[[i]])
}
#don't want to keep coyping over this line and change 1 to 2, 3, 4, etc. SO we use for loops
#use i as a placeholder

output
```

    ## [[1]]
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.04  1.15
    ## 
    ## [[2]]
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.379  6.89
    ## 
    ## [[3]]
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.81 0.637
    ## 
    ## [[4]]
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.00 0.855

## Let’s try a map

``` r
map(list_norm, mean_sd)
```

    ## $a
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.04  1.15
    ## 
    ## $b
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.379  6.89
    ## 
    ## $c
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.81 0.637
    ## 
    ## $d
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.00 0.855

``` r
# give it an input list and a fn i want to define. it gives the output a name

output = map(list_norm, mean_sd)
# output and map(sfsf) are the same thing.
```

What if you want a diff fn?

``` r
output = map(list_norm, median)
```

``` r
output = map_dbl(list_norm, median, .id = "input")
#map double gives me a vector 
```

``` r
output = map_df(list_norm, mean_sd, .id = "input")
#can't make this a dbl bc the output are not individual number. 
#CAN make it a data frame
```

## List columns\!

I want something to keep track of what output matches what input.

``` r
listcol = 
  tibble(
    name = c("a", "b", "c", "d"),
    samp = list_norm
  )
```

``` r
listcol %>% pull(name)
```

    ## [1] "a" "b" "c" "d"

``` r
listcol %>% pull(samp)
```

    ## $a
    ##  [1] 3.6640013 3.8394707 5.0302817 4.3031567 3.1240245 2.4951346 3.6455500
    ##  [8] 1.4496586 4.3472226 2.5129448 2.6227573 2.4577284 3.8664397 3.6117810
    ## [15] 3.8701177 1.4513888 1.7286617 3.7641359 2.5299405 0.5280561
    ## 
    ## $b
    ##  [1]  -0.01368748  -1.85324681  10.95292340  -4.87212862  -9.37810912
    ##  [6]   2.41886739   7.90229183   1.82222685   0.81277929  -9.13053634
    ## [11]   3.88831638   3.72249851  -1.75942473   6.37499915   9.14673566
    ## [16]  -9.27788815   5.90053417   8.37755269   3.02435059   3.46101885
    ## [21]   9.37248659  -9.48593735  -0.82686026   5.09782153  -9.62205517
    ## [26]   8.02479974   2.72814783  -8.92176918 -12.60891755  -3.90084881
    ## 
    ## $c
    ##  [1] 6.823863 7.448474 7.570824 5.841022 7.017768 7.455869 5.217131 6.802839
    ##  [9] 6.543813 7.478909 7.125215 7.179918 6.833974 6.857625 7.402836 5.150440
    ## [17] 6.005091 5.676838 6.385484 8.430348 6.652222 7.095361 6.640248 7.238926
    ## [25] 6.876053 6.929894 6.535965 6.898350 6.575252 7.206467 6.803136 6.934304
    ## [33] 6.166704 6.994951 6.433262 7.290498 7.137239 7.619602 6.781857 6.532246
    ## 
    ## $d
    ##  [1] -2.676720 -3.524387 -1.875402 -3.070962 -2.280767 -2.787047 -2.419002
    ##  [8] -4.314094 -2.597613 -2.590990 -1.829274 -2.640614 -2.114904 -2.782131
    ## [15] -3.734329 -4.588318 -2.805544 -4.376762 -4.332943 -2.588895 -3.370863
    ## [22] -1.999950 -2.781997 -3.565840 -3.357355 -2.695253 -3.269262 -3.078175
    ## [29] -1.578155 -1.023197 -2.452556 -3.381013 -3.007679 -2.586079 -2.822294
    ## [36] -4.353032 -2.108793 -3.227078 -2.910682 -1.675322 -2.522573 -4.623132
    ## [43] -3.137879 -4.243848 -3.735748 -2.049175 -4.394520 -2.489312 -3.730363
    ## [50] -3.705087

``` r
listcol %>% 
  filter(name == "a")
```

    ## # A tibble: 1 x 2
    ##   name  samp        
    ##   <chr> <named list>
    ## 1 a     <dbl [20]>

Let’s try some operations

``` r
listcol$samp #$ is the same as pulling
```

    ## $a
    ##  [1] 3.6640013 3.8394707 5.0302817 4.3031567 3.1240245 2.4951346 3.6455500
    ##  [8] 1.4496586 4.3472226 2.5129448 2.6227573 2.4577284 3.8664397 3.6117810
    ## [15] 3.8701177 1.4513888 1.7286617 3.7641359 2.5299405 0.5280561
    ## 
    ## $b
    ##  [1]  -0.01368748  -1.85324681  10.95292340  -4.87212862  -9.37810912
    ##  [6]   2.41886739   7.90229183   1.82222685   0.81277929  -9.13053634
    ## [11]   3.88831638   3.72249851  -1.75942473   6.37499915   9.14673566
    ## [16]  -9.27788815   5.90053417   8.37755269   3.02435059   3.46101885
    ## [21]   9.37248659  -9.48593735  -0.82686026   5.09782153  -9.62205517
    ## [26]   8.02479974   2.72814783  -8.92176918 -12.60891755  -3.90084881
    ## 
    ## $c
    ##  [1] 6.823863 7.448474 7.570824 5.841022 7.017768 7.455869 5.217131 6.802839
    ##  [9] 6.543813 7.478909 7.125215 7.179918 6.833974 6.857625 7.402836 5.150440
    ## [17] 6.005091 5.676838 6.385484 8.430348 6.652222 7.095361 6.640248 7.238926
    ## [25] 6.876053 6.929894 6.535965 6.898350 6.575252 7.206467 6.803136 6.934304
    ## [33] 6.166704 6.994951 6.433262 7.290498 7.137239 7.619602 6.781857 6.532246
    ## 
    ## $d
    ##  [1] -2.676720 -3.524387 -1.875402 -3.070962 -2.280767 -2.787047 -2.419002
    ##  [8] -4.314094 -2.597613 -2.590990 -1.829274 -2.640614 -2.114904 -2.782131
    ## [15] -3.734329 -4.588318 -2.805544 -4.376762 -4.332943 -2.588895 -3.370863
    ## [22] -1.999950 -2.781997 -3.565840 -3.357355 -2.695253 -3.269262 -3.078175
    ## [29] -1.578155 -1.023197 -2.452556 -3.381013 -3.007679 -2.586079 -2.822294
    ## [36] -4.353032 -2.108793 -3.227078 -2.910682 -1.675322 -2.522573 -4.623132
    ## [43] -3.137879 -4.243848 -3.735748 -2.049175 -4.394520 -2.489312 -3.730363
    ## [50] -3.705087

``` r
mean_sd(listcol$samp[[1]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.04  1.15

``` r
mean_sd(listcol$samp[[2]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.379  6.89

Can I jusp map??

``` r
map(listcol$samp, mean_sd)
```

    ## $a
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.04  1.15
    ## 
    ## $b
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.379  6.89
    ## 
    ## $c
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.81 0.637
    ## 
    ## $d
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.00 0.855

``` r
#now this is a list col inside a data frame, which we are mapping across lists 
```

So can I add a list column?? Yes\!\!\!\! mutate prob\!\!\!

``` r
listcol = listcol %>% 
  mutate(
    summary = map(samp, mean_sd), #don't need $ bc we're in mutate and piped
  medians = map_dbl(samp, median))
```

## Weather Data

``` r
weather_df = 
  rnoaa::meteo_pull_monitors( 
    c("USW00094728", "USC00519397", "USS0023B17S"), 
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2017-01-01",
    date_max = "2017-12-31") %>%
    mutate(
    name = recode(
      id, 
      USW00094728 = "CentralPark_NY", 
      USC00519397 = "Waikiki_HA",
      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) %>% 
  select(name, id, everything())
```

    ## Registered S3 method overwritten by 'hoardr':
    ##   method           from
    ##   print.cache_info httr

    ## using cached file: /Users/madhusudhanpatel/Library/Caches/R/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2020-10-05 13:38:35 (7.522)

    ## file min/max dates: 1869-01-01 / 2020-10-31

    ## using cached file: /Users/madhusudhanpatel/Library/Caches/R/noaa_ghcnd/USC00519397.dly

    ## date created (size, mb): 2020-10-05 13:38:42 (1.699)

    ## file min/max dates: 1965-01-01 / 2020-03-31

    ## using cached file: /Users/madhusudhanpatel/Library/Caches/R/noaa_ghcnd/USS0023B17S.dly

    ## date created (size, mb): 2020-10-05 13:38:44 (0.88)

    ## file min/max dates: 1999-09-01 / 2020-10-31

Get our list columns..

``` r
weather_nest = 
  weather_df %>%
  #nest: i want one df for cp, one for waikiki and one for waterhole
  nest(data = date:tmin)
```

``` r
weather_nest %>% pull(name)
```

    ## [1] "CentralPark_NY" "Waikiki_HA"     "Waterhole_WA"

``` r
weather_nest %>% pull(data)
```

    ## [[1]]
    ## # A tibble: 365 x 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0   8.9   4.4
    ##  2 2017-01-02    53   5     2.8
    ##  3 2017-01-03   147   6.1   3.9
    ##  4 2017-01-04     0  11.1   1.1
    ##  5 2017-01-05     0   1.1  -2.7
    ##  6 2017-01-06    13   0.6  -3.8
    ##  7 2017-01-07    81  -3.2  -6.6
    ##  8 2017-01-08     0  -3.8  -8.8
    ##  9 2017-01-09     0  -4.9  -9.9
    ## 10 2017-01-10     0   7.8  -6  
    ## # … with 355 more rows
    ## 
    ## [[2]]
    ## # A tibble: 365 x 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0  26.7  16.7
    ##  2 2017-01-02     0  27.2  16.7
    ##  3 2017-01-03     0  27.8  17.2
    ##  4 2017-01-04     0  27.2  16.7
    ##  5 2017-01-05     0  27.8  16.7
    ##  6 2017-01-06     0  27.2  16.7
    ##  7 2017-01-07     0  27.2  16.7
    ##  8 2017-01-08     0  25.6  15  
    ##  9 2017-01-09     0  27.2  15.6
    ## 10 2017-01-10     0  28.3  17.2
    ## # … with 355 more rows
    ## 
    ## [[3]]
    ## # A tibble: 365 x 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01   432  -6.8 -10.7
    ##  2 2017-01-02    25 -10.5 -12.4
    ##  3 2017-01-03     0  -8.9 -15.9
    ##  4 2017-01-04     0  -9.9 -15.5
    ##  5 2017-01-05     0  -5.9 -14.2
    ##  6 2017-01-06     0  -4.4 -11.3
    ##  7 2017-01-07    51   0.6 -11.5
    ##  8 2017-01-08    76   2.3  -1.2
    ##  9 2017-01-09    51  -1.2  -7  
    ## 10 2017-01-10     0  -5   -14.2
    ## # … with 355 more rows

``` r
weather_nest$data[[1]]
```

    ## # A tibble: 365 x 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0   8.9   4.4
    ##  2 2017-01-02    53   5     2.8
    ##  3 2017-01-03   147   6.1   3.9
    ##  4 2017-01-04     0  11.1   1.1
    ##  5 2017-01-05     0   1.1  -2.7
    ##  6 2017-01-06    13   0.6  -3.8
    ##  7 2017-01-07    81  -3.2  -6.6
    ##  8 2017-01-08     0  -3.8  -8.8
    ##  9 2017-01-09     0  -4.9  -9.9
    ## 10 2017-01-10     0   7.8  -6  
    ## # … with 355 more rows

Suppose U want to regress `tmax` and `tmin` for each station

``` r
#THIS WORKS
lm(tmax ~ tmin, data = weather_nest$data[[1]]) #"~" is against tmax against (~) tmin
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest$data[[1]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039

A function to do this regression

``` r
weather_lm = function(df) {
  lm(tmax ~ tmin, data = df)
}
#weather_lm(weather_nest$data[[1]]) THIS FN CHECK WORKS
# I want the linear regression for all the data frames (HI and WA) too, so make a for loop

output = vector("list", 3)
for (i in 1:3) {
  output[[i]] = weather_lm(weather_nest$data[[i]])
}
```

What about map??

``` r
map(weather_nest$data, weather_lm) #for each of the data tibbles, i want to apply my weather_lm fn 
```

    ## [[1]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039  
    ## 
    ## 
    ## [[2]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     20.0966       0.4509  
    ## 
    ## 
    ## [[3]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.499        1.221

What about a map in a list column??

``` r
weather_nest %>% 
  mutate(models = map(data, weather_lm))
```

    ## # A tibble: 3 x 4
    ##   name           id          data               models
    ##   <chr>          <chr>       <list>             <list>
    ## 1 CentralPark_NY USW00094728 <tibble [365 × 4]> <lm>  
    ## 2 Waikiki_HA     USC00519397 <tibble [365 × 4]> <lm>  
    ## 3 Waterhole_WA   USS0023B17S <tibble [365 × 4]> <lm>

``` r
# we created a new list of the linear models.

#weather_nest$models #why does this not work??
```
