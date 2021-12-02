Advent of Code 2021
================

  - [Day 1](#day-1)

Here’s my work on Advent of Code 2021.

# Day 1

## Part 1

``` r
d1_sonar <- readLines(here::here("data/day01.txt")) %>% 
    as.double()
d1_L <- length(d1_sonar)
sum(d1_sonar[seq_len(d1_L - 1)] < d1_sonar[seq_len(d1_L - 1) + 1])
```

    ## [1] 1475

## Part 2

``` r
d1_idx <- seq_len(d1_L - 3)
(
  (d1_sonar[d1_idx] + d1_sonar[d1_idx + 1] + d1_sonar[d1_idx + 2]) < 
    (d1_sonar[d1_idx + 1] + d1_sonar[d1_idx + 2] + d1_sonar[d1_idx + 3])
) %>% 
  sum()
```

    ## [1] 1516
