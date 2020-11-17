Writing Functions
================
Nidhi Patel
11/16/2020

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

## Do something simple

``` r
x_vec = rnorm(30, mean = 5, sd = 3)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  1.48491738  0.63206354 -1.79999764 -1.25013027 -0.77305734  1.02575651
    ##  [7] -1.33693142 -0.36600831 -1.12116731  0.42778654 -0.18757887 -0.18699588
    ## [13]  0.07868097  0.63614222 -1.18099218 -0.82708181  0.29865038  0.70947590
    ## [19]  0.67971832  1.17406247 -1.29363172  0.43418091  1.93664755  0.85371262
    ## [25]  1.08942230 -0.89590380  1.23738880  0.25045219 -1.15481699 -0.57476504

``` r
#The above is the z-score
```

The summary numbers here show the z-scores.

I want a function to compute z score

``` r
  #everything that happens between these{} brackets is the body of my function.
#The input of the fn is x, we named it already
z_score = function(x) {
 
 z = (x - mean(x)) / sd(x)
  
  return(z)
  
}
# we named the z-score z, the return is z
z_score(x_vec)
```

    ##  [1]  1.48491738  0.63206354 -1.79999764 -1.25013027 -0.77305734  1.02575651
    ##  [7] -1.33693142 -0.36600831 -1.12116731  0.42778654 -0.18757887 -0.18699588
    ## [13]  0.07868097  0.63614222 -1.18099218 -0.82708181  0.29865038  0.70947590
    ## [19]  0.67971832  1.17406247 -1.29363172  0.43418091  1.93664755  0.85371262
    ## [25]  1.08942230 -0.89590380  1.23738880  0.25045219 -1.15481699 -0.57476504

Try my fn on some other things. These should giver errors

``` r
z_score(3)
```

    ## [1] NA

``` r
#can compute the mean of 3, but cannot compute the sd, need more numbers to compute sd
z_score("my name is nidhi")
```

    ## Warning in mean.default(x): argument is not numeric or logical: returning NA

    ## Error in x - mean(x): non-numeric argument to binary operator

``` r
#can't do this either; cant take mean of character
z_score(mtcars)
```

    ## Warning in mean.default(x): argument is not numeric or logical: returning NA

    ## Error in is.data.frame(x): 'list' object cannot be coerced to type 'double'

``` r
#can't take the z-score of a data set
z_score(c(TRUE, TRUE, FALSE, TRUE))
```

    ## [1]  0.5  0.5 -1.5  0.5

``` r
#this works bc it is logical and coerces it to 0's and 1's.
#each of these is something that causes my fn to break. 
```

Update my fn

``` r
z_score = function(x) {
 if (!is.numeric(x)) {
   stop("Input must be numeric")
 }
  if (length(x) < 3) {
    stop("Input must have at least three numbers")
  }
 z = (x - mean(x)) / sd(x)
  
  return(z)
}

z_score(x_vec)
```

    ##  [1]  1.48491738  0.63206354 -1.79999764 -1.25013027 -0.77305734  1.02575651
    ##  [7] -1.33693142 -0.36600831 -1.12116731  0.42778654 -0.18757887 -0.18699588
    ## [13]  0.07868097  0.63614222 -1.18099218 -0.82708181  0.29865038  0.70947590
    ## [19]  0.67971832  1.17406247 -1.29363172  0.43418091  1.93664755  0.85371262
    ## [25]  1.08942230 -0.89590380  1.23738880  0.25045219 -1.15481699 -0.57476504

Try fn again, should also give errors

``` r
z_score(3)
```

    ## Error in z_score(3): Input must have at least three numbers

``` r
z_score("my name is nidhi")
```

    ## Error in z_score("my name is nidhi"): Input must be numeric

``` r
z_score(mtcars)
```

    ## Error in z_score(mtcars): Input must be numeric

``` r
z_score(c(TRUE, TRUE, FALSE, TRUE))
```

    ## Error in z_score(c(TRUE, TRUE, FALSE, TRUE)): Input must be numeric

``` r
#get input must be numeric!!!!!
```
