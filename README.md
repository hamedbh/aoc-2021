Advent of Code 2021
================

  - [Day 1](#day-1)
  - [Day 2](#day-2)

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

# Day 2

## Part 1

``` r
d2_sub <- tibble(input = readLines(here::here("data/day02.txt"))) %>% 
  separate(input, c("direction", "value"), sep = " ", convert = TRUE)

d2_sub %>% 
  mutate(
    axis = if_else(
      direction %in% c("forward", "back"), 
      "horizontal", 
      "vertical"
    ), 
    sign = if_else(
      direction %in% c("forward", "down"), 
      1L, 
      -1L
    )
  ) %>% 
  group_by(axis) %>% 
  summarise(position = sum(sign * value), .groups = "drop") %>% 
  summarise(total = prod(position))
```

    ## # A tibble: 1 × 1
    ##     total
    ##     <dbl>
    ## 1 1636725

## Part 2

NB. Turns out there are no `back` steps in the input, which is why the
instructions make sense.

``` r
d2_sub %>% 
  count(direction)
```

    ## # A tibble: 3 × 2
    ##   direction     n
    ##   <chr>     <int>
    ## 1 down        373
    ## 2 forward     417
    ## 3 up          210

``` r
d2_sub %>% 
  mutate(
    aim = cumsum(
      ((direction == "down") * value) - ((direction == "up") * value)
    ), 
    forward = (direction == "forward")
  ) %>% 
  mutate(
    horizontal = cumsum(forward * value), 
    depth = cumsum(forward * aim * value)
  ) %>% 
  slice_tail(n = 1) %>% 
  transmute(answer = horizontal * depth)
```

    ## # A tibble: 1 × 1
    ##       answer
    ##        <int>
    ## 1 1872757425
