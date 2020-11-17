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

    ##  [1]  0.112499872  0.454133610 -0.667437285 -1.304245435 -0.255887723
    ##  [6]  0.035414417 -1.287214064  0.570694828 -0.009475162 -0.358077496
    ## [11]  0.298618708 -0.006453437  0.492998488 -0.326360625  1.306112932
    ## [16]  0.454509208  1.545022327  1.247631166 -0.320270049  0.347574264
    ## [21] -1.022339181  1.295513835 -0.481923113  2.309781899 -1.192563566
    ## [26]  0.562077401 -0.145723394 -2.209516728 -1.713057001  0.267961304

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

    ##  [1]  0.112499872  0.454133610 -0.667437285 -1.304245435 -0.255887723
    ##  [6]  0.035414417 -1.287214064  0.570694828 -0.009475162 -0.358077496
    ## [11]  0.298618708 -0.006453437  0.492998488 -0.326360625  1.306112932
    ## [16]  0.454509208  1.545022327  1.247631166 -0.320270049  0.347574264
    ## [21] -1.022339181  1.295513835 -0.481923113  2.309781899 -1.192563566
    ## [26]  0.562077401 -0.145723394 -2.209516728 -1.713057001  0.267961304

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

    ##  [1]  0.112499872  0.454133610 -0.667437285 -1.304245435 -0.255887723
    ##  [6]  0.035414417 -1.287214064  0.570694828 -0.009475162 -0.358077496
    ## [11]  0.298618708 -0.006453437  0.492998488 -0.326360625  1.306112932
    ## [16]  0.454509208  1.545022327  1.247631166 -0.320270049  0.347574264
    ## [21] -1.022339181  1.295513835 -0.481923113  2.309781899 -1.192563566
    ## [26]  0.562077401 -0.145723394 -2.209516728 -1.713057001  0.267961304

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
    ## 1  2.46  4.17

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
    ## 1  2.82  3.51

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
    ## 1  5.91  2.74

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
    ## 1  3.97  2.05

## Let’s review Napopleon Dynamite

``` r
napdyn_url = "https://www.amazon.com/Napoleon-Dynamite-Jon-Heder/product-reviews/B000I9U972/ref=cm_cr_getr_d_paging_btm_prev_1?ie=UTF8&reviewerType=all_reviews&pageNumber=1"

napoleon_html = read_html(napdyn_url)

review_titles = napoleon_html %>% 
  html_nodes(".a-text-bold span") %>% 
  html_text()

review_stars = napoleon_html %>% 
  html_nodes("#cm_cr-review_list .review-rating") %>% 
  html_text() %>% 
  str_extract("^\\d") %>% #this takes out the first digit
  as.numeric()

review_text = napoleon_html %>% 
  html_nodes(".review-text-content span") %>% 
  html_text() %>% 
  str_replace_all("\n", "") %>% #removes the \n at the beginning and end and replaces it with nothing
  str_trim()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

What about the next page of reviews? I can copy and paste the code and
change url to next page to get tibble of the next one. Avoid this
tediousness? Create a function\!\!\!

``` r
read_page_rev = function(url) {
    html = read_html(url)

review_titles = html %>% 
  html_nodes(".a-text-bold span") %>% 
  html_text()

review_stars = html %>% 
  html_nodes("#cm_cr-review_list .review-rating") %>% 
  html_text() %>% 
  str_extract("^\\d") %>% 
  as.numeric()

review_text = html %>% 
  html_nodes(".review-text-content span") %>% 
  html_text() %>% 
  str_replace_all("\n", "") %>% 
  str_trim()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)

reviews
}
```

Let’s try out my function

``` r
napdyn_url = "https://www.amazon.com/Napoleon-Dynamite-Jon-Heder/product-reviews/B000I9U972/ref=cm_cr_getr_d_paging_btm_prev_1?ie=UTF8&reviewerType=all_reviews&pageNumber=1"

read_page_rev(napdyn_url)
```

    ## # A tibble: 10 x 3
    ##    title                           stars text                                   
    ##    <chr>                           <dbl> <chr>                                  
    ##  1 A special memory provided thro…     5 "I try to get my son to watch quality …
    ##  2 The one, the only.                  5 "Some people will not like this film. …
    ##  3 Classic                             5 "The first time I watched it, I was li…
    ##  4 My experiences with Napoleon D…     5 "I live in Preston, Idaho and I am a n…
    ##  5 Umm...                              1 "This was spectacularly stupid. Ended …
    ##  6 A Classic                           5 "Stupid but brilliant. Hilarious but t…
    ##  7 What's it like to be That Guy?      5 "All the schools I attended had somebo…
    ##  8 The ultimate high school outca…     4 "Napoleon Dynamite was the ultimate ne…
    ##  9 16 years later it’s still affe…     4 "A modern, off the wall, classic. See …
    ## 10 silly.. always makes me laugh       5 "Freaking hilarious. It is one of thos…

Let’s read a few pages of reviews

With the function we can just change the url to every page and let it
run the full fn

``` r
napdyn_url_base = "https://www.amazon.com/Napoleon-Dynamite-Jon-Heder/product-reviews/B000I9U972/ref=cm_cr_getr_d_paging_btm_prev_1?ie=UTF8&reviewerType=all_reviews&pageNumber="

#url base is the url up to the page number

napdyn_urls = str_c(napdyn_url_base, 1:5)

napdyn_urls[1] #gives first page
```

    ## [1] "https://www.amazon.com/Napoleon-Dynamite-Jon-Heder/product-reviews/B000I9U972/ref=cm_cr_getr_d_paging_btm_prev_1?ie=UTF8&reviewerType=all_reviews&pageNumber=1"

``` r
napdyn_urls[3] #gives third page
```

    ## [1] "https://www.amazon.com/Napoleon-Dynamite-Jon-Heder/product-reviews/B000I9U972/ref=cm_cr_getr_d_paging_btm_prev_1?ie=UTF8&reviewerType=all_reviews&pageNumber=3"

``` r
more_reviews = bind_rows(
  read_page_rev(napdyn_urls[1]),
  read_page_rev(napdyn_urls[2]),
  read_page_rev(napdyn_urls[3]),
  read_page_rev(napdyn_urls[4]),
  read_page_rev(napdyn_urls[5])
)
#this part is still a little long lol. What if i wanted 30 pages?
```
