find_low_pts <- function(input) {
  n <- nrow(input)

  larger <- rbind(
    rep(Inf, n + 2),
    cbind(rep(Inf, n), input, Inf),
    rep(Inf, n + 2)
  )

  left <- input - larger[seq(2, n + 1), seq(3, n + 2)]
  right <- input - larger[seq(2, n + 1), seq(1, n)]
  up <- input - larger[seq(3, n + 2), seq(2, n + 1)]
  down <- input - larger[seq(1, n), seq(2, n + 1)]

  (left < 0) & (right < 0) & (up < 0) & (down < 0)
}
