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
    ##  [1] 2.549072 3.637541 1.316193 3.322843 2.305538 1.654791 4.194851 2.830606
    ##  [9] 3.635916 2.247433 1.170032 3.048687 2.338020 2.451928 2.915126 2.875103
    ## [17] 2.627629 2.837273 4.141034 3.723498
    ## 
    ## $b
    ##  [1]  4.30463059 -0.84288341  6.40109133  0.36085616 -2.26448381 -4.67729328
    ##  [7] -4.82730528 -0.12526534 -1.97337918 -9.34155937  0.07671905  1.80771069
    ## [13] -4.21745812 -1.72835880  2.02023099  3.91300755  1.66310251 -2.96907030
    ## [19] -5.46560617  2.29680797  8.82837962  3.36813553 -9.96134797  4.00572938
    ## [25]  0.31296250 -7.84451796 -1.69858459  3.45457954  8.05749337  0.41305367
    ## 
    ## $c
    ##  [1] 7.171145 7.191452 7.476304 6.386774 7.247483 7.173360 6.848869 6.998473
    ##  [9] 7.002478 7.342175 6.926888 7.154490 6.189533 7.169716 7.534480 7.000238
    ## [17] 7.171983 6.083443 8.358332 7.390147 6.841390 6.338124 6.966365 6.991889
    ## [25] 7.615807 7.319101 7.391024 8.285078 7.382887 6.421813 6.921842 7.884959
    ## [33] 5.749679 6.197606 6.226919 6.203624 6.694802 8.207071 6.884088 6.106059
    ## 
    ## $d
    ##  [1] -4.8513468 -3.2378748 -1.9863010 -3.0648726 -4.1477940 -1.2130329
    ##  [7] -4.7525139 -1.4574634 -4.4980803 -2.5970943 -2.7108344 -2.3704176
    ## [13] -3.9304516 -4.2147769 -2.6078703 -3.3091458 -4.1789062 -3.2695261
    ## [19] -2.8452163 -4.3368770 -3.3843046 -4.0151473 -3.1512889 -0.9041409
    ## [25] -1.9095270 -2.6604035 -1.6449912 -2.0114388 -2.9691861 -4.3568777
    ## [31] -4.7536302 -3.1298253 -3.1955576 -1.9105632 -1.8091836 -3.7272534
    ## [37] -1.4511965 -3.5307100 -2.5697675 -2.7077259 -3.5261907 -3.5561238
    ## [43] -3.5386803 -2.9725492 -4.1499411 -4.3268730 -2.6980240 -2.0024105
    ## [49] -3.3798784 -2.6907670

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
    ## 1  2.79 0.843

``` r
mean_sd(list_norm[["b"]])
```

    ## # A tibble: 1 x 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.222  4.69

``` r
mean_sd(list_norm[["c"]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  7.01 0.612

``` r
mean_sd(list_norm[["d"]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.08 0.999

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
    ## 1  2.79 0.843
    ## 
    ## [[2]]
    ## # A tibble: 1 x 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.222  4.69
    ## 
    ## [[3]]
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  7.01 0.612
    ## 
    ## [[4]]
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.08 0.999

## Let’s try a map

``` r
map(list_norm, mean_sd)
```

    ## $a
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.79 0.843
    ## 
    ## $b
    ## # A tibble: 1 x 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.222  4.69
    ## 
    ## $c
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  7.01 0.612
    ## 
    ## $d
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.08 0.999

``` r
# give it an input list and a fn i want to define. it gives the output a name

output = map(list_norm, mean_sd)
# output and map(sfsf) are the same thing.
```

What if you want a diff fn?

``` r
output = map(list_norm, median)
```
