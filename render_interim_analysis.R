#!/usr/bin/env Rscript

# Script to render interim-analysis.qmd with different prior_std values
# Creates output directory with date and parameter values in filename

library(quarto)
library(withr)

# Get current date
current_date <- format(Sys.Date(), "%Y%m%d")

# Prior standard deviation values to test
prior_std_values <- c(0.25, 0.5)

# Create output directory
output_dir <- here::here("output")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
quarto_location <- here::here("vignettes", "articles")

# Render for each prior_std value
for (prior_std in prior_std_values) {
  # Create output filename with date and prior_std
  output_file <- file.path(output_dir, paste0("interim_analysis_", current_date, "_prior_std_", prior_std, ".html"))

  cat("Rendering with prior_std =", prior_std, "...\n")
  cat("Output will be saved to:", output_file, "\n")

  # Use withr to temporarily change working directory for rendering
  withr::with_dir(quarto_location, {
    quarto_render(
      input = "interim-analysis.qmd",
      output_file = paste0("interim_analysis_", current_date, "_prior_std_", prior_std, ".html"),
      execute_params = list(prior_std = prior_std, num_workers = 2, total_sims = 250)
    )
  })

  # Move the output file to the output directory
  source_file <- file.path(
    quarto_location,
    paste0("interim_analysis_", current_date, "_prior_std_", prior_std, ".html")
  )
  if (file.exists(source_file)) {
    file.rename(source_file, output_file)
  }

  cat("Completed rendering for prior_std =", prior_std, "\n\n")
}

cat("All renders completed. Outputs saved in:", output_dir, "\n")
