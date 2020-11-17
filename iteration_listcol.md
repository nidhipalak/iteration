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
    ##  [1] 2.0079306 3.2429070 0.3091673 3.2161674 3.0351149 2.0687827 2.8037426
    ##  [8] 2.3230415 2.0038020 3.9442913 1.3712735 2.1318339 2.0328844 1.5799773
    ## [15] 1.2972572 0.8427908 3.0810675 2.0051760 3.1357685 2.2958231
    ## 
    ## $b
    ##  [1]   2.1088215  -0.1651698  -1.4714213  -2.5441040   3.4516005 -12.0412609
    ##  [7]  -2.7930406   0.3931757   1.5745765   0.6247050   2.7452502   3.7448973
    ## [13]  -4.3677084   1.3492116   1.2230318   2.4709241   7.3666867  -1.9593759
    ## [19]   1.4769191  -5.2740397  -0.8480698   0.9966686  -2.9974587   5.5074214
    ## [25]  -8.0714602  -3.9279236   3.2458593  -3.3599643   0.5398248   8.9000026
    ## 
    ## $c
    ##  [1] 7.122603 7.027762 6.747358 7.660193 7.360488 6.767008 7.648541 8.164473
    ##  [9] 7.963305 7.718929 7.195258 7.188372 6.353239 7.913930 7.778084 5.927787
    ## [17] 8.222815 6.661344 7.436761 6.948941 6.897359 6.573470 7.409775 6.350828
    ## [25] 6.229625 6.640359 8.102634 6.337260 6.655949 8.006555 6.224645 6.174552
    ## [33] 7.263348 7.013258 5.778627 5.363273 6.836296 6.808952 6.906679 8.066764
    ## 
    ## $d
    ##  [1] -1.741145 -0.847517 -3.209724 -3.069848 -3.681035 -2.184866 -2.518055
    ##  [8] -3.127696 -1.689071 -3.613181 -3.429029 -2.583687 -2.765016 -4.207588
    ## [15] -2.488526 -3.183684 -3.921804 -4.314184 -3.767461 -4.105421 -2.441307
    ## [22] -3.401447 -3.073192 -2.045788 -3.856085 -3.215686 -4.043224 -3.170340
    ## [29] -3.409039 -1.851621 -2.615073 -1.926826 -2.391113 -2.841229 -1.114474
    ## [36] -1.800229 -2.744586 -1.285503 -3.733806 -3.240110 -2.713401 -2.206531
    ## [43] -3.338877 -1.641471 -4.187742 -2.994736 -3.573882 -3.991036 -4.065848
    ## [50] -1.385953

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
    ## 1  2.24 0.899

``` r
mean_sd(list_norm[["b"]])
```

    ## # A tibble: 1 x 2
    ##      mean    sd
    ##     <dbl> <dbl>
    ## 1 -0.0700  4.32

``` r
mean_sd(list_norm[["c"]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  7.04 0.712

``` r
mean_sd(list_norm[["d"]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.89 0.903

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
    ## 1  2.24 0.899
    ## 
    ## [[2]]
    ## # A tibble: 1 x 2
    ##      mean    sd
    ##     <dbl> <dbl>
    ## 1 -0.0700  4.32
    ## 
    ## [[3]]
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  7.04 0.712
    ## 
    ## [[4]]
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.89 0.903

## Let’s try a map

``` r
map(list_norm, mean_sd)
```

    ## $a
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.24 0.899
    ## 
    ## $b
    ## # A tibble: 1 x 2
    ##      mean    sd
    ##     <dbl> <dbl>
    ## 1 -0.0700  4.32
    ## 
    ## $c
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  7.04 0.712
    ## 
    ## $d
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.89 0.903

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
    ##  [1] 2.0079306 3.2429070 0.3091673 3.2161674 3.0351149 2.0687827 2.8037426
    ##  [8] 2.3230415 2.0038020 3.9442913 1.3712735 2.1318339 2.0328844 1.5799773
    ## [15] 1.2972572 0.8427908 3.0810675 2.0051760 3.1357685 2.2958231
    ## 
    ## $b
    ##  [1]   2.1088215  -0.1651698  -1.4714213  -2.5441040   3.4516005 -12.0412609
    ##  [7]  -2.7930406   0.3931757   1.5745765   0.6247050   2.7452502   3.7448973
    ## [13]  -4.3677084   1.3492116   1.2230318   2.4709241   7.3666867  -1.9593759
    ## [19]   1.4769191  -5.2740397  -0.8480698   0.9966686  -2.9974587   5.5074214
    ## [25]  -8.0714602  -3.9279236   3.2458593  -3.3599643   0.5398248   8.9000026
    ## 
    ## $c
    ##  [1] 7.122603 7.027762 6.747358 7.660193 7.360488 6.767008 7.648541 8.164473
    ##  [9] 7.963305 7.718929 7.195258 7.188372 6.353239 7.913930 7.778084 5.927787
    ## [17] 8.222815 6.661344 7.436761 6.948941 6.897359 6.573470 7.409775 6.350828
    ## [25] 6.229625 6.640359 8.102634 6.337260 6.655949 8.006555 6.224645 6.174552
    ## [33] 7.263348 7.013258 5.778627 5.363273 6.836296 6.808952 6.906679 8.066764
    ## 
    ## $d
    ##  [1] -1.741145 -0.847517 -3.209724 -3.069848 -3.681035 -2.184866 -2.518055
    ##  [8] -3.127696 -1.689071 -3.613181 -3.429029 -2.583687 -2.765016 -4.207588
    ## [15] -2.488526 -3.183684 -3.921804 -4.314184 -3.767461 -4.105421 -2.441307
    ## [22] -3.401447 -3.073192 -2.045788 -3.856085 -3.215686 -4.043224 -3.170340
    ## [29] -3.409039 -1.851621 -2.615073 -1.926826 -2.391113 -2.841229 -1.114474
    ## [36] -1.800229 -2.744586 -1.285503 -3.733806 -3.240110 -2.713401 -2.206531
    ## [43] -3.338877 -1.641471 -4.187742 -2.994736 -3.573882 -3.991036 -4.065848
    ## [50] -1.385953

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
    ##  [1] 2.0079306 3.2429070 0.3091673 3.2161674 3.0351149 2.0687827 2.8037426
    ##  [8] 2.3230415 2.0038020 3.9442913 1.3712735 2.1318339 2.0328844 1.5799773
    ## [15] 1.2972572 0.8427908 3.0810675 2.0051760 3.1357685 2.2958231
    ## 
    ## $b
    ##  [1]   2.1088215  -0.1651698  -1.4714213  -2.5441040   3.4516005 -12.0412609
    ##  [7]  -2.7930406   0.3931757   1.5745765   0.6247050   2.7452502   3.7448973
    ## [13]  -4.3677084   1.3492116   1.2230318   2.4709241   7.3666867  -1.9593759
    ## [19]   1.4769191  -5.2740397  -0.8480698   0.9966686  -2.9974587   5.5074214
    ## [25]  -8.0714602  -3.9279236   3.2458593  -3.3599643   0.5398248   8.9000026
    ## 
    ## $c
    ##  [1] 7.122603 7.027762 6.747358 7.660193 7.360488 6.767008 7.648541 8.164473
    ##  [9] 7.963305 7.718929 7.195258 7.188372 6.353239 7.913930 7.778084 5.927787
    ## [17] 8.222815 6.661344 7.436761 6.948941 6.897359 6.573470 7.409775 6.350828
    ## [25] 6.229625 6.640359 8.102634 6.337260 6.655949 8.006555 6.224645 6.174552
    ## [33] 7.263348 7.013258 5.778627 5.363273 6.836296 6.808952 6.906679 8.066764
    ## 
    ## $d
    ##  [1] -1.741145 -0.847517 -3.209724 -3.069848 -3.681035 -2.184866 -2.518055
    ##  [8] -3.127696 -1.689071 -3.613181 -3.429029 -2.583687 -2.765016 -4.207588
    ## [15] -2.488526 -3.183684 -3.921804 -4.314184 -3.767461 -4.105421 -2.441307
    ## [22] -3.401447 -3.073192 -2.045788 -3.856085 -3.215686 -4.043224 -3.170340
    ## [29] -3.409039 -1.851621 -2.615073 -1.926826 -2.391113 -2.841229 -1.114474
    ## [36] -1.800229 -2.744586 -1.285503 -3.733806 -3.240110 -2.713401 -2.206531
    ## [43] -3.338877 -1.641471 -4.187742 -2.994736 -3.573882 -3.991036 -4.065848
    ## [50] -1.385953

``` r
mean_sd(listcol$samp[[1]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.24 0.899

``` r
mean_sd(listcol$samp[[2]])
```

    ## # A tibble: 1 x 2
    ##      mean    sd
    ##     <dbl> <dbl>
    ## 1 -0.0700  4.32

Can I jusp map??

``` r
map(listcol$samp, mean_sd)
```

    ## $a
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.24 0.899
    ## 
    ## $b
    ## # A tibble: 1 x 2
    ##      mean    sd
    ##     <dbl> <dbl>
    ## 1 -0.0700  4.32
    ## 
    ## $c
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  7.04 0.712
    ## 
    ## $d
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.89 0.903

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
