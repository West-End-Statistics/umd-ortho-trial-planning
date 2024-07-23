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
