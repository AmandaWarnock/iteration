Iteration
================
Amanda Warnock

``` r
library(tidyverse)
```

    ## ── Attaching packages ───────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.3     ✓ dplyr   1.0.2
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ──────────────────────────────────────────────────── tidyverse_conflicts() ──
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
x_vec = rnorm(25, mean = 5, sd = 3)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  1.67219971 -0.60941829  1.28417573 -1.74098277  0.76386500  0.55607967
    ##  [7] -0.15956790 -1.38032447  0.46154258 -0.74746412 -0.27616116 -0.11069365
    ## [13]  0.80820054 -0.96511410 -0.89547125 -0.15347187  0.39595039  0.83186753
    ## [19]  0.49186449 -0.57258818  0.73885815 -1.24379004  2.15006749  0.08492134
    ## [25] -1.38454478

I want a function to compute z scores.

``` r
z_scores = function(x) {
  
  z = (x - mean(x)) / sd(x)
  return(z)
  
}

z_scores(x_vec)
```

    ##  [1]  1.67219971 -0.60941829  1.28417573 -1.74098277  0.76386500  0.55607967
    ##  [7] -0.15956790 -1.38032447  0.46154258 -0.74746412 -0.27616116 -0.11069365
    ## [13]  0.80820054 -0.96511410 -0.89547125 -0.15347187  0.39595039  0.83186753
    ## [19]  0.49186449 -0.57258818  0.73885815 -1.24379004  2.15006749  0.08492134
    ## [25] -1.38454478

try my function on some other things.

``` r
z_scores(3)
```

    ## [1] NA

^NA

``` r
z_scores("my name is amanda")
```

    ## Warning in mean.default(x): argument is not numeric or logical: returning NA

    ## Error in x - mean(x): non-numeric argument to binary operator

^error

``` r
z_scores(iris)
```

    ## Warning in mean.default(x): argument is not numeric or logical: returning NA

    ## Warning in Ops.factor(left, right): '-' not meaningful for factors

    ## Error in is.data.frame(x): 'list' object cannot be coerced to type 'double'

^cant coerce to type double

``` r
z_scores(sample(c(TRUE, FALSE), 25, replace = TRUE))
```

    ##  [1]  0.9413574 -1.0198039  0.9413574  0.9413574  0.9413574  0.9413574
    ##  [7]  0.9413574  0.9413574 -1.0198039  0.9413574 -1.0198039  0.9413574
    ## [13] -1.0198039 -1.0198039  0.9413574 -1.0198039 -1.0198039  0.9413574
    ## [19] -1.0198039  0.9413574 -1.0198039 -1.0198039 -1.0198039 -1.0198039
    ## [25]  0.9413574

^coerces it, still wrong though

update function to make it break if used wrong

``` r
z_scores = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Z scores cannot be computed for length 1 vectors")
  }
  
  z = mean(x) / sd(x)
  
  z
}
```

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Cannot be computed for length 1 vectors")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)

  list(mean = mean_x, 
       sd = sd_x)
}
```

check that it works

``` r
x_vec = rnorm(100, mean = 3, sd = 4)
mean_and_sd(x_vec)
```

    ## $mean
    ## [1] 2.911003
    ## 
    ## $sd
    ## [1] 3.732171

## multiple inputs

``` r
sim_data = tibble(
  x = rnorm(n = 100, mean = 4, sd = 3)
)

sim_data %>% 
  summarize(
    mu_hat = mean(x),
    sigma_hat = sd(x)
  )
```

    ## # A tibble: 1 x 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   4.38      3.05

id like to do this ^ w/ a function

``` r
sim_mean_sd = function(n, mu, sigma) {
  
  sim_data = tibble(
    x = rnorm(n, mean = mu, sd = sigma),
  )
  
  sim_data %>% 
    summarize(
      mu_hat = mean(x),
      sigma_hat = sd(x)
    )
}

sim_mean_sd(100, 6, 3)
```

    ## # A tibble: 1 x 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   6.15      3.21

``` r
sim_mean_sd(mu = 6, n = 100, sigma = 3)
```

    ## # A tibble: 1 x 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   6.73      3.07

(mu and sigma bc true pop parameters) (this is positional matching,
named matching can also be used and generally should be) if you set the
originals to numbers, they will be overridden during matching if you
specify it

## revisit nap dynamite

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_nodes(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text() %>%
  str_extract("^\\d") %>%
  as.numeric()

review_text = 
  dynamite_html %>%
  html_nodes(".review-text-content span") %>%
  html_text() %>% 
  str_replace_all("\n", "") %>% 
  str_trim()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

what about the next page of reviews though? time to turn into function.

``` r
read_page_reviews <- function(url) {
  
  html = read_html(url)
  
  review_titles = 
    html %>%
    html_nodes(".a-text-bold span") %>%
    html_text()
  
  review_stars = 
    html %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text() %>%
    str_extract("^\\d") %>%
    as.numeric()
  
  review_text = 
    html %>%
    html_nodes(".review-text-content span") %>%
    html_text() %>% 
    str_replace_all("\n", "") %>% 
    str_trim()

reviews =   
  tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text
  )
}
```

try it

``` r
url_base = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="
vec_urls = str_c(url_base, 1:5)

dynamite_reviews = bind_rows(
  read_page_reviews(vec_urls[1]),
  read_page_reviews(vec_urls[2]),
  read_page_reviews(vec_urls[3]),
  read_page_reviews(vec_urls[4]),
  read_page_reviews(vec_urls[5])
)

dynamite_reviews
```

    ## # A tibble: 50 x 3
    ##    title                  stars text                                            
    ##    <chr>                  <dbl> <chr>                                           
    ##  1 Vote for Pedro!            5 Just watch the movie. Gosh!                     
    ##  2 Just watch the freaki…     5 Its a great movie, gosh!!                       
    ##  3 Great Value                5 Great Value                                     
    ##  4 I LOVE THIS MOVIE          5 THIS MOVIE IS SO FUNNY ONE OF MY FAVORITES      
    ##  5 Don't you wish you co…     5 Watch it 100 times. Never. Gets. Old.           
    ##  6 Stupid, but very funn…     5 If you like stupidly funny '90s teenage movies …
    ##  7 The beat                   5 The best                                        
    ##  8 Hilarious                  5 Super funny! Loved the online rental.           
    ##  9 Love this movie            5 We love this product.  It came in a timely mann…
    ## 10 Entertaining, limited…     4 Entertainment level gets a 5 star but having pr…
    ## # … with 40 more rows

can remove global environment by restarting r to make sure that its
reading in just what you want it to

## mean scoping example

``` r
f = function(x) {
  z = x + y
  z
}

x = 1
y = 2

f(x = y)
```

    ## [1] 4

got 4 bc of how r does scoping –\> goes searching for y and finds it in
the global environment bc its not in the function

## function as arguments

``` r
x_vec = rnorm(25, 0, 1)

my_summary = function(x, summ_func) {
  summ_func(x)
}

my_summary(x_vec, sd)
```

    ## [1] 0.9344964
