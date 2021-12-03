Advent of Code 2021
================

  - [Day 1](#day-1)
  - [Day 2](#day-2)
  - [Day 3](#day-3)

Here’s my work on Advent of Code 2021.

# Day 1

## Part 1

``` r
d1_sonar <- scan(here::here("data/day01.txt"))
sum(diff(d1_sonar) > 0)
```

    ## [1] 1475

## Part 2

``` r
d1_idx <- seq_len(length(d1_sonar) - 3)
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
d2_sub <- read_delim(
  "data/day02.txt", 
  delim = " ", 
  col_names = c("direction", "value"), 
  col_types = cols(col_character(), col_integer())
)

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

# Day 3

## Part 1

``` r
d3_input <- readLines(here::here("data/day03.txt")) %>% 
  str_split("") %>% 
  map(as.integer) %>% 
  reduce(rbind)
  
d3_sums <- (colSums(d3_input) > (nrow(d3_input) / 2))
d3_get_prod <- function(x, y) {
  map(
  list(x, y), 
  ~ .x %>% 
    as.integer() %>% 
    as.character() %>% 
    str_c(collapse = "") %>% 
    strtoi(base = 2L)
  ) %>% 
  reduce(prod)
}
d3_get_prod(d3_sums, !d3_sums)
```

    ## [1] 2972336

## Part 2

``` r
d3_oxygen <- d3_input
for (j in seq_len(ncol(d3_input))) {
  most_common <- as.integer(sum(d3_oxygen[, j]) >= (nrow(d3_oxygen) / 2))
  d3_oxygen <- d3_oxygen[d3_oxygen[, j] == most_common, ]
  if (is.null(dim(d3_oxygen))) {
    break
  }
}

d3_co2 <- d3_input
for (j in seq_len(ncol(d3_input))) {
  least_common <- as.integer(!(sum(d3_co2[, j]) >= (nrow(d3_co2) / 2)))
  d3_co2 <- d3_co2[d3_co2[, j] == least_common, ]
  if (is.null(dim(d3_co2))) {
    break
  }
}
d3_get_prod(d3_oxygen, d3_co2)
```

    ## [1] 3368358
