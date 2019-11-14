Cross validation
================
Stephanie Zhen
11/12/2019

``` r
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.2.1     v purrr   0.3.2
    ## v tibble  2.1.3     v dplyr   0.8.3
    ## v tidyr   1.0.0     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.4.0

    ## -- Conflicts ---------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(modelr)
library(mgcv)
```

    ## Loading required package: nlme

    ## 
    ## Attaching package: 'nlme'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse

    ## This is mgcv 1.8-28. For overview type 'help("mgcv-package")'.

``` r
set.seed(1)
```

Generate a dataset

``` r
nonlin_df = 
  tibble(
    id = 1:100,
    x = runif(100, 0, 1),
    y = 1 - 10 * (x - .3) ^ 2 + rnorm(100, 0, .3)
  )

nonlin_df %>% 
  ggplot(aes(x = x, y = y)) + 
  geom_point() + theme_bw() 
```

![](cross_validation_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

Training and testing

``` r
train_df = sample_n(nonlin_df, 80)
test_df = anti_join(nonlin_df, train_df, by = "id")

ggplot(train_df, aes(x = x, y = y)) + 
  geom_point() + 
  geom_point(data = test_df, color = "red")
```

![](cross_validation_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

Fit three models if varying goodness:

``` r
linear_mod = lm(y ~ x, data = train_df)
smooth_mod = mgcv::gam(y ~ s(x), data = train_df)
wiggly_mod = mgcv::gam(y ~ s(x, k = 30), sp = 10e-6, data = train_df)
```

Let’s look at some fit:

``` r
linear_train = train_df %>% 
  add_predictions(linear_mod) %>% 
  ggplot(aes(x = x, y = y)) + geom_point() + 
  geom_line(aes(y = pred), color = "red")

smooth_train = train_df %>% 
  add_predictions(smooth_mod) %>% 
  ggplot(aes(x = x, y = y)) + geom_point() + 
  geom_line(aes(y = pred), color = "red")

wiggly_train = train_df %>% 
  add_predictions(wiggly_mod) %>% 
  ggplot(aes(x = x, y = y)) + geom_point() + 
  geom_line(aes(y = pred), color = "red")

linear_train
```

![](cross_validation_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
smooth_train
```

![](cross_validation_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

``` r
wiggly_train
```

![](cross_validation_files/figure-gfm/unnamed-chunk-5-3.png)<!-- -->

``` r
train_df %>% 
  gather_predictions(linear_mod, smooth_mod, wiggly_mod) %>% 
  mutate(model = fct_inorder(model)) %>% 
  ggplot(aes(x = x, y = y)) + 
  geom_point() + 
  geom_line(aes(y = pred), color = "red") + 
  facet_wrap(~model)
```

![](cross_validation_files/figure-gfm/unnamed-chunk-5-4.png)<!-- -->

Looking at RMSE

``` r
rmse(linear_mod, test_df)
```

    ## [1] 0.7052956

``` r
rmse(smooth_mod, test_df)
```

    ## [1] 0.2221774

``` r
rmse(wiggly_mod, test_df)
```

    ## [1] 0.289051

## Do all of this using modelr

``` r
cv_df = 
  crossv_mc(nonlin_df, 100)
## Repeat 100 times for different 80/20 splits. 
```

One note about resample:

``` r
cv_df %>% pull(train) %>% .[[1]] %>% as_tibble
```

    ## # A tibble: 79 x 3
    ##       id      x       y
    ##    <int>  <dbl>   <dbl>
    ##  1     1 0.266   1.11  
    ##  2     2 0.372   0.764 
    ##  3     3 0.573   0.358 
    ##  4     4 0.908  -3.04  
    ##  5     6 0.898  -1.99  
    ##  6     7 0.945  -3.27  
    ##  7     8 0.661  -0.615 
    ##  8     9 0.629   0.0878
    ##  9    10 0.0618  0.392 
    ## 10    11 0.206   1.63  
    ## # ... with 69 more rows

``` r
## in my cv_df, pull first training split. All the resamples are different from each other.  
## makes this more memory efficient
```

``` r
cv_df =
  cv_df %>% 
  mutate(
    train = map(train, as_tibble),
    test = map(test, as_tibble))

## Instead of keeping each dataset as row number in the dataset, we expand the actual list of the each resample object, Makes it less memory intensive, but make use it to make models.  
```

Try fitting the linear model in all of these:

``` r
cv_df = 
  cv_df %>% 
  mutate(linear_mods  = map(train, ~lm(y ~ x, data = .x)),
         smooth_mods  = map(train, ~mgcv::gam(y ~ s(x), data = .x)),
         wiggly_mods  = map(train, ~gam(y ~ s(x, k = 30), sp = 10e-6, data = .x))) %>% 
  mutate(rmse_linear = map2_dbl(linear_mods, test, ~rmse(model = .x, data = .y)),
         rmse_smooth = map2_dbl(smooth_mods, test, ~rmse(model = .x, data = .y)),
         rmse_wiggly = map2_dbl(wiggly_mods, test, ~rmse(model = .x, data = .y)))
```

``` r
cv_df %>% 
  select(starts_with("rmse")) %>% 
  pivot_longer(
    everything(),
    names_to = "model", 
    values_to = "rmse",
    names_prefix = "rmse_") %>% 
  mutate(model = fct_inorder(model)) %>% 
  ggplot(aes(x = model, y = rmse)) + geom_violin()
```

![](cross_validation_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->
