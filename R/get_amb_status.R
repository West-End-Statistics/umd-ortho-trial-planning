get_amb_status <- function(
    x,
    unable_to_walk = .07,
    walk_w_human = .37,
    walk_w_aid = .34) {
  cut_to_use <- qnorm(
    cumsum(
      c(unable_to_walk, walk_w_human, walk_w_aid)
    )
  )

  cut(x,
    c(-Inf, cut_to_use, Inf),
    labels = c(
      "unable to walk",
      "walk with human assistance",
      "walk with aid",
      "walk without assistance"
    )
  )
}
