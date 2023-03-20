jbsMisc
================

## Installation

Run the command:

    remotes::install_github("jb-santos/svyEffects")

You’ll need the `{remotes}` package to do so, which can be installed by
running:

    install.packages("remotes")

## Simple crosstab using `ctab()`

Create a survey design object. (These functions also work on vanilla
data frames).

``` r
library(tidyverse)
library(survey)
data("ab")
absvy <- svydesign(~1, data = ab, weights = ~weight)
```

Generate the crosstab using `ctab()`.

``` r
library(jbsMisc)
table_vote <- ctab(absvy, yvar = "dvote3", xvar = "region")
table_vote
#> # A tibble: 12 × 4
#> # Groups:   region [4]
#>    dvote3 region       n   pct
#>    <fct>  <fct>    <dbl> <dbl>
#>  1 UCP    Calgary    224 0.586
#>  2 NDP    Calgary     97 0.254
#>  3 Other  Calgary     61 0.160
#>  4 UCP    Edmonton   143 0.402
#>  5 NDP    Edmonton   136 0.382
#>  6 Other  Edmonton    77 0.216
#>  7 UCP    Other      208 0.619
#>  8 NDP    Other       74 0.220
#>  9 Other  Other       54 0.161
#> 10 UCP    Total      576 0.536
#> 11 NDP    Total      307 0.286
#> 12 Other  Total      191 0.178
```

The output is in long, or “tidy” format and lends itself well to
plotting with `ggplot()`.

``` r
library(ggplot2)
ggplot(table_vote) +
  aes(x = region, y = pct, fill = dvote3) +
  geom_bar(stat = "identity")
```

![](man/figures/unnamed-chunk-5-1.png)<!-- -->

``` r
library(ggplot2)
ggplot(table_vote) +
  aes(x = region, y = pct, fill = dvote3) +
  geom_bar(stat = "identity", position = position_dodge())
```

![](man/figures/unnamed-chunk-6-1.png)<!-- -->

## Multiple crosstabs (aka “stub-and-banner” tables) using `ctabs()`

We can generate stub-and-banner crosstabs that cross-tabulate a single
“dependent” (or row) variable against multiple “independent” (columns)
variables using the `ctabs()` function. This is similar to the output in
many polling reports (the generation of which is what led me to write
these functions).

Calculate the crosstabs. Multiple column variables can be specified by
naming multiple variables within the argument `xvars = c()`.

The output is a list object with the class `ctabs`.

``` r
library(jbsMisc)
table_vote1 <- ctabs(absvy, yvar = "dvote3", xvars = c("region", "female", "age"))
names(table_vote1)
#> [1] "Summary" "Total"   "region"  "female"  "age"
```

The first slot `$Summary` is the multiple crosstab table.

``` r
table_vote1
#> # A tibble: 3 × 11
#>   dvote3 Total Calgary Edmonton Other Male  Female `18-24` `25-44` `45-64` `65+`
#>   <chr>  <chr> <chr>   <chr>    <chr> <chr> <chr>  <chr>   <chr>   <chr>   <chr>
#> 1 UCP    54%   59%     40%      62%   57%   50%    46%     52%     57%     57%  
#> 2 NDP    29%   25%     38%      22%   26%   31%    32%     28%     28%     29%  
#> 3 Other  18%   16%     22%      16%   17%   19%    22%     20%     15%     14%
```

The second slot `$Total` contains the marginal frequency distribution of
the row variable.

``` r
table_vote1[[2]]
#> # A tibble: 3 × 4
#>   dvote3 Total     n   pct
#>   <fct>  <fct> <dbl> <dbl>
#> 1 UCP    Total   576 0.536
#> 2 NDP    Total   307 0.286
#> 3 Other  Total   191 0.178
```

Subsequent tables contain a long- or tidy-format crosstab between the
row variable and the respective column variable.

``` r
table_vote1[[4]]
#> # A tibble: 6 × 4
#> # Groups:   female [2]
#>   dvote3 female     n   pct
#>   <fct>  <fct>  <dbl> <dbl>
#> 1 UCP    Male     313 0.568
#> 2 NDP    Male     144 0.261
#> 3 Other  Male      94 0.171
#> 4 UCP    Female   263 0.503
#> 5 NDP    Female   163 0.312
#> 6 Other  Female    97 0.186
```

### Plotting multiple crosstabs

There is a `plot()` method included for the objects outputted by
`ctabs()`.

``` r
plot(table_vote1)
```

![](man/figures/unnamed-chunk-11-1.png)<!-- -->

The `plot()` function can take a few arguments to modify the default
plots.

`plot.ctabs(x, tblno = 1, dodge = FALSE, txt = FALSE)`

- `tblno =` selects which table in the `ctabs` list object to plot.
- `dodge =` whether bars should be “clustered” using `position_dodge()`.
  By default, they are stacked using `position_stack()`.
- `txt =` should text labels be added to graph.

``` r
plot(table_vote1, dodge = TRUE, txt = TRUE)
```

![](man/figures/unnamed-chunk-12-1.png)<!-- -->

`plot()` is a wrapper for `ggplot()`, so you can add additional options
using `+ ...`.

``` r
plot(table_vote1, dodge = FALSE, txt = TRUE) +
  scale_fill_manual(values = c("#184484", "#f37221", "#bbbbbb")) +
  labs(title = "Alberta vote intention",
       subtitle = "Decided/leaning voters only, n=1,074",
       y = "",
       x = "Sub-group",
       fill = "Party",
       caption = "Janet Brown Opinion Research / Trend Research") +
  theme_classic()
```

![](man/figures/unnamed-chunk-13-1.png)<!-- -->

### Plotting other tables in `ctabs()` objects

The second slot has the marginal frequencies.

``` r
plot(table_vote1, 2, dodge = TRUE) + theme_bw()
```

![](man/figures/unnamed-chunk-14-1.png)<!-- -->

Slots 3 onward each have a plot of the row variable against the
respective column variable.

``` r
plot(table_vote1, 5, dodge = TRUE) + theme_bw()
```

![](man/figures/unnamed-chunk-15-1.png)<!-- -->

## Testing columns for significant differences using `testcols()`

This function tests columns for significant differences. You can also
obtain Holm-corrected p-values for multiple comparisons using the
argument `adj.p = TRUE`.

For now, it outputs a list object where each slot is a data frame of the
test result summaries (i.e. significance stars). If/when I find some
time in the future, I’ll update the output to integrate the crosstab
with the significance markings.

``` r
table_vote1 <- ctabs(absvy, yvar = "dvote3", xvars = c("region", "education"))
table_vote1_tests <- testcols(table_vote1)
```

``` r
table_vote1_tests
#> $region
#>       Calgary - Edmonton Calgary - Other Edmonton - Other
#> UCP                    *                                *
#> NDP                    *                                *
#> Other                                                    
#> 
#> $education
#>       HS/less - Some PSE HS/less - Bachelors HS/less - Graduate
#> UCP                                        *                  *
#> NDP                                        *                  *
#> Other                                                          
#>       Some PSE - Bachelors Some PSE - Graduate Bachelors - Graduate
#> UCP                      *                   *                    *
#> NDP                      *                   *                    *
#> Other
```

If you want to see a crosstab at the same time the significant
differences table, you could do something like this:

``` r
table_vote1[[3]] %>% 
  select(-n) %>%
  pivot_wider(names_from = region, values_from = pct)
#> # A tibble: 3 × 4
#>   dvote3 Calgary Edmonton Other
#>   <fct>    <dbl>    <dbl> <dbl>
#> 1 UCP      0.586    0.402 0.619
#> 2 NDP      0.254    0.382 0.220
#> 3 Other    0.160    0.216 0.161
table_vote1_tests[[1]]
#>       Calgary - Edmonton Calgary - Other Edmonton - Other
#> UCP                    *                                *
#> NDP                    *                                *
#> Other
```

Another example with education versus vote choice:

``` r
table_vote1[[4]] %>% 
  select(-n) %>%
  pivot_wider(names_from = education, values_from = pct)
#> # A tibble: 3 × 5
#>   dvote3 `HS/less` `Some PSE` Bachelors Graduate
#>   <fct>      <dbl>      <dbl>     <dbl>    <dbl>
#> 1 UCP        0.684      0.608     0.453    0.309
#> 2 NDP        0.161      0.204     0.373    0.510
#> 3 Other      0.155      0.188     0.173    0.181
table_vote1_tests[[2]]
#>       HS/less - Some PSE HS/less - Bachelors HS/less - Graduate
#> UCP                                        *                  *
#> NDP                                        *                  *
#> Other                                                          
#>       Some PSE - Bachelors Some PSE - Graduate Bachelors - Graduate
#> UCP                      *                   *                    *
#> NDP                      *                   *                    *
#> Other
```
