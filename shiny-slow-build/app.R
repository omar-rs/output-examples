library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(randomForest)
library(DT)

# Heavy startup computation — intended to push build past 60s to generate
# periodic usage-frame ticks in the vivid-blender pipeline.
set.seed(42)
n <- 50000

cat("Simulating dataset...\n")
sim_data <- data.frame(
  x1 = rnorm(n),
  x2 = runif(n),
  x3 = rexp(n),
  x4 = rbeta(n, 2, 5),
  x5 = rnorm(n, mean = 3, sd = 1.5),
  x6 = rpois(n, lambda = 4),
  x7 = rnorm(n) * runif(n),
  x8 = cumsum(rnorm(n)) / seq_len(n)
)
sim_data$y <- factor(ifelse(
  0.4 * sim_data$x1 - 0.3 * sim_data$x2 + 0.2 * sim_data$x3 +
    rnorm(n, sd = 0.5) > 0.3,
  "A", "B"
))

cat("Fitting random forest (500 trees)...\n")
rf_model <- randomForest(y ~ ., data = sim_data, ntree = 500, importance = TRUE)

importance_df <- as.data.frame(importance(rf_model)) |>
  tibble::rownames_to_column("feature") |>
  arrange(desc(MeanDecreaseGini))

cat("Startup complete.\n")

ui <- page_sidebar(
  title = "Slow-Build Shiny — RF Explorer",
  theme = bs_theme(bootswatch = "cosmo"),
  sidebar = sidebar(
    sliderInput("ntree_disp", "Trees to display in OOB plot", 10, 500, 100, step = 10),
    selectInput("feature_x", "X axis (scatter)", choices = names(sim_data)[1:8], selected = "x1"),
    selectInput("feature_y", "Y axis (scatter)", choices = names(sim_data)[1:8], selected = "x2"),
    numericInput("sample_n", "Scatter sample size", value = 500, min = 100, max = 5000, step = 100)
  ),
  layout_columns(
    col_widths = c(6, 6, 12),
    card(
      card_header("Variable Importance"),
      plotOutput("importance_plot")
    ),
    card(
      card_header("OOB Error vs Trees"),
      plotOutput("oob_plot")
    ),
    card(
      card_header("Scatter — coloured by class"),
      plotOutput("scatter_plot")
    )
  ),
  card(
    card_header("Importance Table"),
    DTOutput("importance_table")
  )
)

server <- function(input, output, session) {
  output$importance_plot <- renderPlot({
    ggplot(importance_df, aes(x = reorder(feature, MeanDecreaseGini), y = MeanDecreaseGini)) +
      geom_col(fill = "#4e79a7") +
      coord_flip() +
      labs(x = NULL, y = "Mean Decrease Gini") +
      theme_minimal()
  })

  output$oob_plot <- renderPlot({
    oob <- data.frame(
      trees = seq_len(input$ntree_disp),
      error = rf_model$err.rate[seq_len(input$ntree_disp), "OOB"]
    )
    ggplot(oob, aes(x = trees, y = error)) +
      geom_line(colour = "#e15759") +
      labs(x = "Number of trees", y = "OOB error rate") +
      theme_minimal()
  })

  output$scatter_plot <- renderPlot({
    samp <- sim_data[sample(nrow(sim_data), input$sample_n), ]
    ggplot(samp, aes(x = .data[[input$feature_x]], y = .data[[input$feature_y]], colour = y)) +
      geom_point(alpha = 0.5, size = 1.2) +
      scale_colour_manual(values = c(A = "#4e79a7", B = "#e15759")) +
      theme_minimal()
  })

  output$importance_table <- renderDT({
    datatable(importance_df, options = list(pageLength = 8), rownames = FALSE)
  })
}

shinyApp(ui, server)
