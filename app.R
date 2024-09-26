library(shiny)
library(bslib)
library(BuyseTest)
source("R/evaluate_dataset.R")
source("R/get_amb_status.R")
source("R/make_summary_table.R")
source("R/simulate_data.R")
source("R/simulate_trial.R")
r_files <- list.files(here::here("R"), full.names = TRUE)
sapply(r_files, source)


ui <- fluidPage(
  titlePanel("Ortho Trial Simulations"),
  sidebarLayout(
    sidebarPanel(
      width = 5,
      navset_card_pill(
        title = "Parameters",
        nav_panel(
          "Data Generation",
          fluidRow(
            column(
              4,
              h4("Placebo Arm"),
              numericInput("placebo_survival", "Survival Rate", value = .85),
              numericInput("placebo_dah_mean", "Days at Home (mean)", value = 67),
              numericInput("placebo_dah_sd", "Days at Home (sd)", value = 20),
              numericInput("placebo_unable_to_walk", "Ambulatory: Unable to walk", value = .07),
              numericInput("placebo_walk_w_human", "Ambulatory: Walk with human", value = .37),
              numericInput("placebo_walk_w_aid", "Ambulatory: Walk with aid", value = .34),
            ),
            column(
              4,
              h4("Active Arm"),
              numericInput("active_survival", "Survival Rate", value = .90),
              numericInput("active_dah_mean", "Days at Home (mean)", value = 78),
              numericInput("active_dah_sd", "Days at Home (sd)", value = 20),
              numericInput("active_latent_shift", "Ambulatory: Latent Shift", value = .2)
            ),
            column(
              4,
              h4("General Parameters"),
              numericInput("corr_died_daysathome",
                "Correlation: Survival and Days at Home",
                value = .5,
                min = -1, max = 1
              ),
              numericInput("corr_died_ambulation",
                "Correlation: Survival and Ambulation Status",
                value = .5,
                min = -1, max = 1
              ),
              numericInput("corr_days_ambulation",
                "Correlation: Days at Home and Ambulation",
                value = .5,
                min = -1, max = 1
              ),
              h4("Missing Rates"),
              numericInput("perc_missing_surv", "Survival",
                value = 0,
                min = 0, max = 1
              ),
              numericInput("perc_missing_amb", "Ambulation",
                value = 0,
                min = 0, max = 1
              ),
              numericInput("perc_missing_dah", "Days at Home",
                value = 0,
                min = 0, max = 1
              )
            )
          )
        ),
        nav_panel(
          "Simulation",
          sliderInput("n_per_arm",
            "Observations per arm",
            min = 10, max = 1000, value = 150,
            width = "90%",
          ),
          numericInput("number_sims", "Number of Simulations", min = 2, max = 1e4, step = 1, value = 10),
          numericInput("amb_status_thresh", "Amb. Status Threshold", value = 1, min = 1, max = 4),
          numericInput("dah_thresh", "Days at home Threshold", value = 7, min = 1),
        ),
        nav_panel(
          "Statistics",
          p(
            "Two-sided value should be double the one-sided.
          Ex. a two-sided number of .05 should give similar results to one-sided treament = .025"
          ),
          numericInput("alpha", "Alpha:", min = .01, max = .2, step = .01, value = .05),
          radioButtons("alpha_comparison", "Comparison", choices = c(
            "Two-sided" = "both",
            "One-sided: Treatment" = "treatment",
            "One-sided: Control" = "control"
          ))
        )
      )
    ),
    mainPanel(
      width = 7,
      navset_card_pill(
        title = "Data Simulation Ouputs",
        nav_panel(
          "Simulation Check",
          actionButton(
            "button_check_params",
            "Check Params"
          ),
          verbatimTextOutput("corr_summary"),
          gt::gt_output("trial_summary"),
        ),
        nav_panel(
          "Power Analysis",
          actionButton("button_run_sims", "Run Simultions"),
          gt::gt_output("power_summary"),
          p(
            "First three rows show components of winratio",
            br(),
            "Last row of win ratio (days_at_home) shows overall win for trial",
            br(),
            "Final three rows show univariate models"
          )
        )
      )
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
      ),
      survival_missing = input$perc_missing_surv,
      days_at_home_missing = input$perc_missing_dah,
      amb_status_missing = input$perc_missing_amb
    )
    r$corr_data <- d_cust_corr[, c("died", "days_at_home", "amb_status_numeric")] |>
      cor(method = "spearman", use = "pairwise.complete.obs")
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
      ),
      survival_missing = input$perc_missing_surv,
      days_at_home_missing = input$perc_missing_dah,
      amb_status_missing = input$perc_missing_amb
    )

    out <- make_summary_tbl(
      d
    )
    r$trial_summary <- gtsummary::as_gt(out)
  })

  observeEvent(input$button_run_sims, {
    message("Running ", input$number_sims, " simultions...")
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = paste0("Running ", input$number_sims, " simulations..."), value = 0)
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
        ),
        survival_missing = input$perc_missing_surv,
        days_at_home_missing = input$perc_missing_dah,
        amb_status_missing = input$perc_missing_amb
      )

      output <- evaluate_dataset(d,
        alpha = input$alpha,
        amb_status_thresh = input$amb_status_thresh,
        days_at_home_thresh = input$dah_thresh,
        alpha_comparison = input$alpha_comparison
      )
      if (.i %% 10 == 0) {
        progress$inc(1 / input$number_sims, detail = paste0(.i, "/", input$number_sims))
      } else {
        progress$inc(1 / input$number_sims)
      }

      output
    }) |>
      purrr::list_rbind() |>
      dplyr::group_by(model, term) |>
      dplyr::summarise(
        dplyr::across(c(estimate, win), mean),
        sims = dplyr::n(), .groups = "drop"
      ) |>
      dplyr::mutate(
        term = gsub("_t[0-9]+$", "", term),
        model = factor(
          model,
          c("winratio", "death", "ambulation_status", "days_at_home")
        ),
        term = factor(
          term,
          c("died", "amb_status_numeric", "days_at_home")
        )
      ) |>
      dplyr::arrange(model, term) |>
      gt::gt() |>
      gt::sub_missing(missing_text = "-") |>
      gt::fmt_number(columns = estimate:win)


    r$power_summary <- out
  })


  output$power_summary <- gt::render_gt(r$power_summary)
  output$trial_summary <- gt::render_gt(r$trial_summary)
  output$corr_summary <- renderPrint(r$corr_data)
}
shinyApp(ui, server)
