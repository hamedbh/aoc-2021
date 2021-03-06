Advent of Code 2021
================

  - [Day 1](#day-1)
  - [Day 2](#day-2)
  - [Day 3](#day-3)
  - [Day 4](#day-4)
  - [Day 5](#day-5)
  - [Day 6](#day-6)
  - [Day 7](#day-7)
  - [Day 8](#day-8)

``` r
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✔ ggplot2 3.3.5     ✔ purrr   0.3.4
    ## ✔ tibble  3.1.6     ✔ dplyr   1.0.7
    ## ✔ tidyr   1.1.4     ✔ stringr 1.4.0
    ## ✔ readr   2.1.1     ✔ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
walk(list.files(here::here("R"), full.names = TRUE), source)
```

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

# Day 4

## Part 1

``` r
d4_input <- readLines(here::here("data/day04.txt"))

d4_order <- d4_input[[1]] %>% 
  str_split(",") %>% 
  pluck(1) %>% 
  as.integer()

d4_numbers <- d4_input[map(0:99, ~ 3:7 + (6 * .x)) %>% unlist() %>% sort()] %>%
  str_trim() %>% 
  str_split(" +") %>% 
  unlist() %>% 
  as.integer()

d4_cards <- d4_numbers %>% 
  array(dim = c(5, 5, 100))

d4_position <- match(d4_numbers, d4_order) %>% 
  array(dim = c(5, 5, 100))

d4_rows <- d4_position %>% 
  apply(MARGIN = c(1, 3), FUN = max) %>% 
  apply(MARGIN = 2, FUN = min)
d4_cols <- d4_position %>% 
  apply(MARGIN = c(2, 3), FUN = max) %>% 
  apply(MARGIN = 2, FUN = min)
d4_win_times <- pmin(d4_rows, d4_cols)

d4_scores <- (
  (
  d4_cards * (
    d4_position > (
      rep(d4_win_times, each = 25) %>% 
        array(dim = c(5, 5, 100))
    )
  )
) %>% 
  apply(MARGIN = 3, sum)
) * (d4_order[d4_win_times])

d4_scores[which.min(d4_win_times)]
```

    ## [1] 2496

## Part 2

``` r
d4_scores[which.max(d4_win_times)]
```

    ## [1] 25925

# Day 5

## Part 1

``` r
d5_input <- tibble(
  raw = readLines(here::here("data/day05.txt"))
) %>% 
  separate(
    raw, 
    into = c("x1", "y1", "x2", "y2"), 
    convert = TRUE
  ) %>% 
  rowid_to_column() %>% 
  mutate(
    direction = case_when(
      x1 == x2 ~ "vertical", 
      y1 == y2 ~ "horizontal", 
      TRUE     ~ "other"
    )
  ) %>% 
  mutate(
    all_pts = pmap(
      ., 
      ~ str_c(seq(..2, ..4), ",", seq(..3, ..5))
    )
  )
```

``` r
d5_input %>% 
  filter(direction != "other") %>% 
  select(rowid, all_pts) %>% 
  unnest(all_pts) %>% 
  group_by(all_pts) %>% 
  summarise(pt_count = n_distinct(rowid)) %>% 
  count(pt_count > 1)
```

    ## # A tibble: 2 × 2
    ##   `pt_count > 1`      n
    ##   <lgl>           <int>
    ## 1 FALSE          101541
    ## 2 TRUE             5147

## Part 2

``` r
d5_input %>% 
  select(rowid, all_pts) %>% 
  unnest(all_pts) %>% 
  group_by(all_pts) %>% 
  summarise(pt_count = n_distinct(rowid)) %>% 
  count(pt_count > 1)
```

    ## # A tibble: 2 × 2
    ##   `pt_count > 1`      n
    ##   <lgl>           <int>
    ## 1 FALSE          152072
    ## 2 TRUE            16925

# Day 6

## Part 1

Brute force worked fine for Part 1 but then died on Part 2. Left here
for posterity.

    d6_fish <- scan(here::here("data/day06.txt"), what = integer(), sep = ",")
    
    d6_increment_fish <- function(timer) {
      if (timer == 0L) {
        return(c(6L, 8L))
      } else {
        return(timer - 1L)
      }
    }
    d6_sim_fish <- function(timers, N) {
      res <- timers
      for (n in seq_len(N)) {
        res <- map(res, d6_increment_fish) %>% 
          unlist()
      }
      res
    }
    d6_sim_fish(d6_fish, N = 80L) %>% 
      length()

Need to switch to keeping track of counts of fish.

``` r
d6_fish <- scan(here::here("data/day06.txt"), what = double(), sep = ",")
# There are no zero values in the initial states, so need to add that to the
# front
d6_counts <- c(0, tabulate(d6_fish, 8))
```

``` r
d6_sim_fish <- function(counts, N) {
  res <- counts
  for (n in seq_len(N)) {
    res <- c(res[2:9], res[1]) + c(rep(0, 6), res[1], rep(0, 2))
  }
  res
}
d6_sim_fish(d6_counts, N = 80) %>% 
  sum()
```

    ## [1] 395627

## Part 2

``` r
options(scipen = 999)
d6_sim_fish(d6_counts, N = 256) %>% 
  sum()
```

    ## [1] 1767323539209

# Day 7

## Part 1

``` r
d7_input <- scan(here::here("data/day07.txt"), sep = ",")
sum(abs(d7_input - median(d7_input)))
```

    ## [1] 356992

## Part 2

``` r
d7_increase_fuel_cost <- function(n) {
  (n * (n + 1)) / 2
}
d7_all_poss_dist <- seq(min(d7_input), max(d7_input))
d7_fuel_cost <- map_dbl(
  d7_all_poss_dist, 
  ~ sum(d7_increase_fuel_cost(abs(d7_input - .x)))
)
d7_fuel_cost[which.min(d7_fuel_cost)]
```

    ## [1] 101268110

# Day 8

## Part 1

``` r
d8_input <- read_delim(
  here::here("data/day08.txt"),
  delim = " | ",
  col_names = c("signals", "output"),
  col_types = cols(.default = col_character())
) %>%
  mutate(across(.fns = str_trim))

d8_input %>% 
  mutate(
    counts = map(
      output, 
      ~ str_split(.x, " ") %>% 
        pluck(1) %>% 
        map_int(nchar)
    )
  ) %>% 
  mutate(
    total = map_int(
      counts, 
      ~ sum(.x %in% c(2L, 4L, 3L, 7L))
    )
  ) %>% 
  summarise(answer = sum(total))
```

    ## # A tibble: 1 × 1
    ##   answer
    ##    <int>
    ## 1    294

## Part 2

Easier to wrap the required code in a function.

``` r
parse_display
```

    ## function (signals, output) 
    ## {
    ##     sig_len <- lengths(signals)
    ##     check_setdiff <- function(signals, digit, len = 1) {
    ##         map_int(signals, ~length(setdiff(signals[[digit]], .x))) == 
    ##             1
    ##     }
    ##     d1 <- which(sig_len == 2)
    ##     d4 <- which(sig_len == 4)
    ##     d7 <- which(sig_len == 3)
    ##     d8 <- which(sig_len == 7)
    ##     d6 <- which(check_setdiff(signals, d1) & sig_len == 6)
    ##     d0 <- which(check_setdiff(signals, d4) & sig_len == 6) %>% 
    ##         setdiff(d6)
    ##     d9 <- which(sig_len == 6) %>% setdiff(d6) %>% setdiff(d0)
    ##     d5 <- which(check_setdiff(signals, d6) & sig_len == 5)
    ##     d3 <- which(check_setdiff(signals, d9) & sig_len == 5) %>% 
    ##         setdiff(d5)
    ##     d2 <- which(sig_len == 5) %>% setdiff(d5) %>% setdiff(d3)
    ##     output %>% match(signals[c(d0, d1, d2, d3, d4, d5, d6, d7, 
    ##         d8, d9)] %>% map(sort)) %>% {
    ##         . - 1
    ##     } %>% str_c(collapse = "") %>% as.integer()
    ## }

``` r
d8_input %>%
  mutate(
    across(
      .fns = ~ str_split(.x, " ") %>%
        map(str_split, "") %>%
        map(map, sort)
    )
  ) %>%
  mutate(parsed = map2_int(signals, output, parse_display)) %>%
  summarise(answer = sum(parsed))
```

    ## # A tibble: 1 × 1
    ##   answer
    ##    <int>
    ## 1 973292
