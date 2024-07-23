library(shiny)
library(BuyseTest)
r_files <- list.files(here::here("R"), full.names = TRUE)
sapply(r_files, source)


ui <- fluidPage(
  titlePanel("Ortho Trial Simulations"),
  sidebarLayout(
    sidebarPanel(
      h4("Placebo Parameters"),
      numericInput("placebo_survival", "Survival Rate", value = .85),
      numericInput("placebo_dah_mean", "Days at Home (mean)", value = 67),
      numericInput("placebo_dah_sd", "Days at Home (sd)", value = 20),
      numericInput("placebo_unable_to_walk", "Ambulatory: Unable to walk", value = .07),
      numericInput("placebo_walk_w_human", "Ambulatory: Walk with human", value = .37),
      numericInput("placebo_walk_w_aid", "Ambulatory: Walk with aid", value = .34),
      h4("Active Parameters"),
      numericInput("active_survival", "Survival Rate", value = .90),
      numericInput("active_dah_mean", "Days at Home (mean)", value = 78),
      numericInput("active_dah_sd", "Days at Home (sd)", value = 20),
      numericInput("active_latent_shift", "Ambulatory: Latent Shift", value = .2),
      h4("General Parameters"),
      numericInput("corr_died_daysathome",
        "Correlation: Survival and Days at Home",
        value = .5
      ),
      numericInput("corr_died_ambulation",
        "Correlation: Survival and Ambulation Status",
        value = .5
      ),
      numericInput("corr_days_ambulation",
        "Correlation: Days at Home and Ambulation",
        value = .5
      ),
      actionButton("button_check_params", "Check Params"),
      h4("Simulation Parameters"),
      sliderInput("n_per_arm",
        "Observations per arm",
        min = 10, max = 1000, value = 150
      ),
      numericInput("number_sims", "Number of Simulations", min = 2, max = 1e4, step = 1, value = 10),
      numericInput("amb_status_thresh", "Amb. Status Threshold", value = 1, min = 1, max = 4),
      numericInput("dah_thresh", "Days at home Threshold", value = 7, min = 1),
      actionButton("button_run_sims", "Run Simultions")
    ),
    mainPanel(
      h4("Data Simulation Check"),
      verbatimTextOutput("corr_summary"),
      gt::gt_output("trial_summary"),
      h4("Power Analysis"),
      verbatimTextOutput("power_summary"),
    )
  )
)
server <- function(input, output, session) {
  r <- reactiveValues(corr_data = NULL)

  observeEvent(input$button_check_params, {
    d_cust_corr <- simulate_data(10000,
      arm = "placebo",
      survival_prob = c(input$placebo_survival, input$active_survival),
      days_at_home_mean = c(input$placebo_dah_mean, input$active_dah_mean),
      days_at_home_sd = c(input$placebo_dah_sd, input$active_dah_sd),
      placebo_amb_status = c(
        input$placebo_unable_to_walk,
        input$placebo_walk_w_human,
        input$placebo_walk_w_aid
      ),
      amb_status_latent_shift = input$active_latent_shift,
      outcome_correlation = custom_outcome_corr(
        input$corr_died_daysathome,
        input$corr_died_ambulation,
        input$corr_days_ambulation
      )
    )
    r$corr_data <- d_cust_corr[, c("died", "days_at_home", "amb_status_numeric")] |>
      cor(method = "spearman")
  })

  observeEvent(input$button_check_params, {
    d <- simulate_trial(10000,
      survival_prob = c(input$placebo_survival, input$active_survival),
      days_at_home_mean = c(input$placebo_dah_mean, input$active_dah_mean),
      days_at_home_sd = c(input$placebo_dah_sd, input$active_dah_sd),
      placebo_amb_status = c(
        input$placebo_unable_to_walk,
        input$placebo_walk_w_human,
        input$placebo_walk_w_aid
      ),
      amb_status_latent_shift = input$active_latent_shift,
      outcome_correlation = custom_outcome_corr(
        input$corr_died_daysathome,
        input$corr_died_ambulation,
        input$corr_days_ambulation
      )
    )

    out <- make_summary_tbl(
      d
    )
    r$trial_summary <- gtsummary::as_gt(out)
  })

  observeEvent(input$button_run_sims, {
    message("Running ", input$number_sims, " simultions...")
    out <- lapply(1:input$number_sims, function(.i) {
      d <- simulate_trial(input$n_per_arm,
        survival_prob = c(input$placebo_survival, input$active_survival),
        days_at_home_mean = c(input$placebo_dah_mean, input$active_dah_mean),
        days_at_home_sd = c(input$placebo_dah_sd, input$active_dah_sd),
        placebo_amb_status = c(
          input$placebo_unable_to_walk,
          input$placebo_walk_w_human,
          input$placebo_walk_w_aid
        ),
        amb_status_latent_shift = input$active_latent_shift,
        outcome_correlation = custom_outcome_corr(
          input$corr_died_daysathome,
          input$corr_died_ambulation,
          input$corr_days_ambulation
        )
      )

      evaluate_dataset(d,
        alpha = .05,
        amb_status_thresh = input$amb_status_thresh,
        days_at_home_thresh = input$dah_thresh
      )
    }) |>
      purrr::list_rbind() |>
      dplyr::summarise(
        dplyr::across(winratio:days, mean),
        sims = dplyr::n()
      )

    r$power_summary <- out
  })





  output$power_summary <- renderPrint(r$power_summary)
  output$trial_summary <- gt::render_gt(r$trial_summary)
  output$corr_summary <- renderPrint(r$corr_data)
}
shinyApp(ui, server)
