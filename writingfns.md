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

    ##  [1]  0.46711041 -0.28995509 -0.41611891  0.70404695  0.45988260 -0.63464035
    ##  [7]  1.16509986 -1.01627804  0.05290674  0.14631020 -0.16486457 -2.22178428
    ## [13]  1.51855675 -0.66453997  1.31168281 -0.08878938 -0.34213461  0.66802043
    ## [19]  1.20236951  0.85993125  1.71082566 -0.88320151 -2.01940271  0.34521414
    ## [25] -0.50090213  0.14810009 -0.02283791  0.43318377  0.17382446 -2.10161619

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

    ##  [1]  0.46711041 -0.28995509 -0.41611891  0.70404695  0.45988260 -0.63464035
    ##  [7]  1.16509986 -1.01627804  0.05290674  0.14631020 -0.16486457 -2.22178428
    ## [13]  1.51855675 -0.66453997  1.31168281 -0.08878938 -0.34213461  0.66802043
    ## [19]  1.20236951  0.85993125  1.71082566 -0.88320151 -2.01940271  0.34521414
    ## [25] -0.50090213  0.14810009 -0.02283791  0.43318377  0.17382446 -2.10161619

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

    ##  [1]  0.46711041 -0.28995509 -0.41611891  0.70404695  0.45988260 -0.63464035
    ##  [7]  1.16509986 -1.01627804  0.05290674  0.14631020 -0.16486457 -2.22178428
    ## [13]  1.51855675 -0.66453997  1.31168281 -0.08878938 -0.34213461  0.66802043
    ## [19]  1.20236951  0.85993125  1.71082566 -0.88320151 -2.01940271  0.34521414
    ## [25] -0.50090213  0.14810009 -0.02283791  0.43318377  0.17382446 -2.10161619

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

## Multiple Outputs

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

Check that fn works

``` r
x_vec = rnorm(100, mean = 3, sd = 4)
mean_sd(x_vec)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.34  4.43

Multiple inputs I’d like to do this with a fn

``` r
sim_data = 
  tibble(
    x = rnorm(100, mean = 3, sd = 4)
  )

sim_data %>% 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.91  4.22

``` r
sim_mean_sd = function(samp_size, mu, sigma) {
  # mu = mean, sigma = sd
  sim_data = 
  tibble(
    x = rnorm(n = samp_size, mean = mu, sd = sigma)
  )

sim_data %>% 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
}

sim_mean_sd(100, 6, 3)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.35  3.01

``` r
# having sim_mean_sd(samp_size = 100, mu = 6, sigma = 3) are NAME MATCHING
#here n =100, mu = 6 and sigma = 3. This creates a dataframe of this normal distribution following the mu and sigma of the sample size
```

``` r
sim_mean_sd = function(samp_size, mu = 6, sigma = 3) {
  # the mu and sigma are defaulted here. When inputting sample size, we can override the default mu and sigma by specifiying mu = x and sigma = y
  sim_data = 
  tibble(
    x = rnorm(n = samp_size, mean = mu, sd = sigma)
  )

sim_data %>% 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
}

sim_mean_sd(100, mu = 4, sigma = 2) # this overrides the original mu and sigma inputs in the function
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.98  2.17
