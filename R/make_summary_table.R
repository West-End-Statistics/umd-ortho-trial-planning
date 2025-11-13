#' Create Summary Table for Trial Data
#'
#' Generates a formatted summary table using gtsummary, stratified by treatment arm.
#'
#' @param d Data frame. Trial dataset with an 'arm' column.
#'
#' @return A gtsummary table object with descriptive statistics by arm.
#'
#' @export
#' @importFrom gtsummary tbl_summary add_stat_label all_continuous all_categorical
make_summary_tbl <- function(d) {
  gtsummary::tbl_summary(d,
    statistic = list(
      gtsummary::all_continuous() ~ "{mean} ({sd})",
      gtsummary::all_categorical() ~ "{n} ({p}%)"
    ),
    by = "arm",
    include = -amb_status_numeric
  ) |> gtsummary::add_stat_label()
}
