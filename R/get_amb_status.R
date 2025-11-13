#' Convert Continuous Latent Variable to Ambulatory Status Categories
#'
#' Transforms a continuous latent variable into ordered ambulatory status
#' categories based on specified proportions.
#'
#' @param x Numeric vector. Continuous latent variable to categorize.
#' @param unable_to_walk Numeric. Proportion unable to walk (default: 0.07).
#' @param walk_w_human Numeric. Proportion walking with human assistance (default: 0.37).
#' @param walk_w_aid Numeric. Proportion walking with aid (default: 0.34).
#'
#' @return Ordered factor with levels: "unable to walk", "walk with human assistance",
#'   "walk with aid", "walk without assistance".
#'
#' @export
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
