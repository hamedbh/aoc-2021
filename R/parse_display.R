parse_display <- function(signals, output) {
  sig_len <- lengths(signals)

  check_setdiff <- function(signals, digit, len = 1) {
    map_int(signals, ~ length(setdiff(signals[[digit]], .x))) == 1
  }


  d1 <- which(sig_len == 2)
  d4 <- which(sig_len == 4)
  d7 <- which(sig_len == 3)
  d8 <- which(sig_len == 7)
  d6 <- which(check_setdiff(signals, d1) & sig_len == 6)
  d0 <- which(check_setdiff(signals, d4) & sig_len == 6) %>%
    setdiff(d6)
  d9 <- which(sig_len == 6) %>%
    setdiff(d6) %>%
    setdiff(d0)
  d5 <- which(check_setdiff(signals, d6) & sig_len == 5)
  d3 <- which(check_setdiff(signals, d9) & sig_len == 5) %>%
    setdiff(d5)
  d2 <- which(sig_len == 5) %>%
    setdiff(d5) %>%
    setdiff(d3)
  output %>%
    match(
      signals[
        c(d0, d1, d2, d3, d4, d5, d6, d7, d8, d9)
      ] %>%
        map(sort)
    ) %>%
    {
      . - 1
    } %>%
    str_c(collapse = "") %>%
    as.integer()
}
