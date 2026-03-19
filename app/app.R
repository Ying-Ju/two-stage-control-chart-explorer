# UPDATED VERSION: UD theme + Design Explorer + Skewness–Correlation Explorer + Learn
library(shiny)
pacman::p_load(bslib, DEoptim, DT, htmltools, patchwork, sn, tidyverse)


# -------------------------------------------------------------------
# Source study functions
# -------------------------------------------------------------------
source("all_functions.R")

# -------------------------------------------------------------------
# Benchmark scenarios from Section 4
# -------------------------------------------------------------------
income_df <- matrix(c(
  2 , 0.01, 1, 150,50, 350, 500, 5.0  , 1.0, 3.05,  4.05, 0.05,
  4 , 0.01, 2, 150,  50, 135, 500, 0.5  , 0.1, 4.00, 41.00, 0.05,
  6 , 0.05, 1,  50, -50, 350,  50, 0.5  , 1.0, 4.00, 41.00, 0.05,
  8 , 0.05, 2,  50, -50, 135,  50, 5.0  , 0.1, 3.05,  4.05, 0.05,
  10, 0.01, 1, 150,  50, 260,  50, 5.0  , 0.1, 4.00,  5.00, 0.50,
  12, 0.01, 2, 150,  50,  45,  50, 0.5  , 1.0, 3.05, 40.05, 0.50,
  14, 0.05, 1,  50, -50, 260, 500, 0.5  , 0.1, 3.05, 40.05, 0.50,
  16, 0.05, 2,  50, -50,  45, 500, 5.0  , 1.0, 4.00,  5.00, 0.50,
  18, 0.01, 1,  50, -50,  45, 500, 5.0  , 1.0, 20.05, 40.05, 0.05,
  20, 0.01, 2,  50, -50, 260, 500, 0.5  , 0.1, 21.00,  5.00, 0.05,
  22, 0.05, 1, 150,  50,  45,  50, 0.5  , 1.0, 21.00,  5.00, 0.05,
  24, 0.05, 2, 150,  50, 260,  50, 5.0  , 0.1, 20.05, 40.05, 0.05,
  26, 0.01, 1,  50, -50, 135,  50, 5.0  , 0.1, 21.00, 41.00, 0.50,
  28, 0.01, 2,  50, -50, 350,  50, 0.5  , 1.0, 20.05,  4.05, 0.50,
  30, 0.05, 1, 150,  50, 135, 500, 0.5  , 0.1, 20.05,  4.05, 0.50,
  32, 0.05, 2, 150,  50, 350, 500, 5.0  , 1.0, 21.00, 41.00, 0.50),
  ncol = 12, nrow = 16, byrow = TRUE
)

income_df <- data.frame(income_df)
colnames(income_df) <- c(
  "example", "theta", "shift_size",
  "i1", "i2",
  "eta1", "eta2", "eta3", "eta4",
  "b1", "b2", "b3"
)

scenario_labels <- c(
  `2`  = "Example 2: high-penalty benchmark",
  `4`  = "Example 4: moderate-cost / large-shift benchmark",
  `6`  = "Example 6: low-reward / negative out-of-control benchmark",
  `8`  = "Example 8: larger shift with low reward benchmark",
  `10` = "Example 10: high net-income benchmark",
  `12` = "Example 12: high in-control return benchmark",
  `14` = "Example 14: low net-income benchmark",
  `16` = "Example 16: low-reward / larger-shift benchmark",
  `18` = "Example 18: high fixed-time-cost benchmark",
  `20` = "Example 20: high false-alarm-cost benchmark",
  `22` = "Example 22: favorable-reward / short-cycle benchmark",
  `24` = "Example 24: favorable-reward / larger-shift benchmark",
  `26` = "Example 26: mixed-cost benchmark",
  `28` = "Example 28: high-penalty / larger-shift benchmark",
  `30` = "Example 30: high fixed-time-cost / short-cycle benchmark",
  `32` = "Example 32: high-penalty / large-shift benchmark"
)

get_scenario_row <- function(example_id) {
  income_df %>% filter(example == example_id)
}

# -------------------------------------------------------------------
# Wrappers
# -------------------------------------------------------------------
run_skew_chart <- function(row_data, beta1, alpha_x, alpha_y, B = 10000) {
  maximize_exp_income_skew(
    beta1 = beta1,
    theta = row_data$theta,
    shift_size = row_data$shift_size,
    b1 = row_data$b1,
    b2 = row_data$b2,
    b3x = row_data$b3,
    i1 = row_data$i1,
    i2 = row_data$i2,
    eta1 = row_data$eta1,
    eta2 = row_data$eta2,
    eta3x = row_data$eta3,
    eta4x = row_data$eta4,
    alpha_x = alpha_x,
    alpha_y = alpha_y,
    B = B
  )
}

run_normal_chart <- function(row_data, beta1, B = 10000) {
  maximize_exp_income_normal(
    beta1 = beta1,
    theta = row_data$theta,
    shift_size = row_data$shift_size,
    b1 = row_data$b1,
    b2 = row_data$b2,
    b3x = row_data$b3,
    i1 = row_data$i1,
    i2 = row_data$i2,
    eta1 = row_data$eta1,
    eta2 = row_data$eta2,
    eta3x = row_data$eta3,
    eta4x = row_data$eta4
  )
}

extract_chart_result <- function(res, label) {
  tibble(
    Chart = label,
    EA = res$EA,
    ATS = res$details$ATS,
    EFA = res$details$EFA,
    nX = round(res$opt_par[1]),
    nY = round(res$opt_par[2]),
    hX = res$opt_par[3],
    hY = res$opt_par[4],
    kX = res$opt_par[5],
    kY = res$opt_par[6],
    wX = res$opt_par[7]
  )
}

run_design_comparison <- function(row_data, beta1, alpha_x, alpha_y, chart_mode = "both", B = 10000) {
  out <- list()
  if (chart_mode %in% c("both", "skew")) {
    out$skew <- extract_chart_result(
      run_skew_chart(row_data, beta1, alpha_x, alpha_y, B),
      "Two-stage skew-normal"
    )
  }
  if (chart_mode %in% c("both", "normal")) {
    out$normal <- extract_chart_result(
      run_normal_chart(row_data, beta1, B),
      "Two-stage normal (Costa)"
    )
  }
  bind_rows(out)
}

# -------------------------------------------------------------------
# Correlation helpers
# NOTE: update compute_cross_chart_correlation() to match your function
# names from all_functions.R if needed.
# -------------------------------------------------------------------
compute_cross_chart_correlation_wrapper <- function(alpha_zeta, alpha_delta, alpha_epsilon, beta1) {
  if (exists("rhoxyfun")) {
    out <- rhoxyfun(
      beta1 = beta1,
      alphazeta = alpha_zeta,
      alphadelta = alpha_delta,
      alphaepsilon = alpha_epsilon
    )
    return(out$rho)
  }
  NA_real_
}

make_design_metric_plot <- function(results_df) {
  long_df <- results_df %>%
    select(Chart, EA, ATS, EFA) %>%
    pivot_longer(cols = c(EA, ATS, EFA), names_to = "Metric", values_to = "Value") %>%
    mutate(
      Metric = factor(Metric, levels = c("EA", "ATS", "EFA"),
                      labels = c("Economic Performance", "Detection Speed", "False Alarm Rate"))
    )
  
  ggplot(long_df, aes(x = Chart, y = Value, fill = Chart)) +
    geom_col(width = 0.62) +
    facet_wrap(~ Metric, scales = "free_y", nrow = 1) +
    labs(x = NULL, y = NULL) +
    scale_fill_manual(values = c(
      "Two-stage skew-normal" = "#C41230",
      "Two-stage normal (Costa)" = "#2E4057"
    )) +
    theme_minimal(base_size = 13) +
    theme(
      strip.text = element_text(face = "bold", size = 15),
      axis.text.x = element_text(angle = 15, hjust = 1, size = 11),
      legend.position = "none",
      panel.grid.major.x = element_blank()
    )
}

make_design_metric_table <- function(results_df) {
  results_df %>%
    select(Chart, EA, ATS, EFA) %>%
    mutate(across(c(EA, ATS, EFA), ~ round(.x, 4)))
}

make_design_parameter_table <- function(results_df) {
  results_df %>%
    select(Chart, nX, nY, hX, hY, kX, kY, wX) %>%
    mutate(across(-Chart, ~ round(.x, 4)))
}

make_correlation_grid <- function(vary_param, fixed_alpha_zeta, fixed_alpha_delta, fixed_alpha_epsilon, fixed_beta1, scan_seq) {
  tibble(scan_value = scan_seq) %>%
    rowwise() %>%
    mutate(
      alpha_zeta = if (vary_param == "alpha_zeta") scan_value else fixed_alpha_zeta,
      alpha_delta = if (vary_param == "alpha_delta") scan_value else fixed_alpha_delta,
      alpha_epsilon = if (vary_param == "alpha_epsilon") scan_value else fixed_alpha_epsilon,
      beta1 = if (vary_param == "beta1") scan_value else fixed_beta1,
      correlation = compute_cross_chart_correlation_wrapper(alpha_zeta, alpha_delta, alpha_epsilon, beta1)
    ) %>%
    ungroup()
}

param_choices <- c(
  "alpha_zeta"    = "&alpha;<sub>&zeta;</sub>",
  "alpha_delta"   = "&alpha;<sub>&delta;</sub>",
  "alpha_epsilon" = "&alpha;<sub>&epsilon;</sub>",
  "beta1"         = "&beta;<sub>1</sub>"
)

# -------------------------------------------------------------------
# Theme
# -------------------------------------------------------------------
app_theme <- bs_theme(
  version = 5,
  primary = "#C41230",
  secondary = "#2E4057",
  success = "#6C9A8B",
  bg = "#F8F8F8",
  fg = "#1F2937",
  base_font = font_google("Inter"),
  heading_font = font_google("Inter")
)

app_footer <- div(
  style = "margin: 24px auto 12px auto; max-width: 96%; background: white; border-radius: 18px; padding: 18px 24px; box-shadow: 0 4px 14px rgba(0,0,0,0.08);",
  fluidRow(
    column(
      width = 3,
      div(
        style = "display:flex; align-items:center; justify-content:center; height:100%;",
        tags$img(src = "UD_logo1.png", style = "max-width: 180px; max-height: 80px; width: auto; height: auto;")
      )
    ),
    column(
      width = 6,
      div(
        style = "text-align:center;",
        div(style = "font-weight:700; font-size: 1.1rem; color:#2E4057; margin-bottom: 6px;", "Two-Stage Control Chart Explorer"),
        div(style = "font-size: 0.98rem; margin-bottom: 6px;", "Based on the paper: Su, N. C., Chen, Y. J. & Huang, W. H., Two-Stage Control Charts for Monitoring Correlated Quality Variables with a Bivariate Skew-Normal Framework"),
        div(style = "font-size: 0.95rem; margin-bottom: 8px;", "App developed by: Ying-Ju (Tessa) Chen"),
        div(style = "display:inline-block; background:#F3EFE6; border-radius:16px; padding:6px 16px; font-weight:600; color:#5B6470;", "Version 0.1.0")
      )
    ),
    column(
      width = 3,
      div(
        style = "display:flex; align-items:center; justify-content:center; height:100%;",
        tags$img(src = "NTPU_logo.png", style = "max-width: 200px; max-height: 80px; width: auto; height: auto;")
      )
    )
  )
)


# -------------------------------------------------------------------
# UI
# -------------------------------------------------------------------
ui <- page_navbar(
  header = tags$head(
    tags$style(HTML("
      .btn-primary {
        background-color: #C41230 !important;
        border-color: #C41230 !important;
        color: white !important;
      }
      .btn-primary:hover, .btn-primary:focus {
        background-color: #A50F2D !important;
        border-color: #A50F2D !important;
        color: white !important;
      }
      .card {
        border-radius: 18px !important;
      }
      .accordion-button:not(.collapsed) {
        color: #1F2937;
        background-color: #FCECEF;
      }
      .navbar {
        min-height: 78px;
      }
      .navbar > .container-fluid {
        display: flex;
        align-items: center;
        gap: 1.5rem;
      }
      .navbar-brand {
        display: flex !important;
        align-items: center;
        margin-right: 2.5rem !important;
        padding-top: 0 !important;
        padding-bottom: 0 !important;
        font-weight: 700;
        line-height: 1.1;
      }
      .navbar-nav {
        display: flex;
        align-items: center;
        gap: 0.5rem;
      }
      .navbar-nav .nav-link {
        padding-top: 1rem !important;
        padding-bottom: 1rem !important;
      }
    "))
  ),
  title = div(style = "font-weight:700; display:flex; align-items:center; height:100%;", "Two-Stage Control Chart Explorer"),
  theme = app_theme,
  bg = "#C41230",
  inverse = TRUE,
  
  nav_panel(
    "Design Explorer",
    page_fluid(
      div(
        class = "p-4 mb-4",
        style = "background: white; border-radius: 18px;",
        h3("Two-Stage Control Chart Design Explorer"),
        p(
          "Explore economically designed two-stage control charts under skew-normal and normal modeling frameworks. ",
          "Choose a benchmark scenario or enter your own economic parameters to compare the resulting designs and performance metrics."
        )
      ),
      layout_sidebar(
        sidebar = card(
          card_header("Inputs"),
          radioButtons(
            "chart_mode", "Chart option",
            choices = c("Compare both" = "both", "Two-stage skew-normal only" = "skew", "Two-stage normal only" = "normal"),
            selected = "both"
          ),
          selectInput(
            "scenario", "Benchmark scenario",
            choices = setNames(income_df$example, scenario_labels[as.character(income_df$example)]),
            selected = 10
          ),
          sliderInput("beta1", HTML("&beta;<sub>1</sub>"), min = 0.1, max = 1.5, value = 0.3, step = 0.1),
          numericInput("alpha_x", HTML("Skewness parameter &alpha;<sub>X</sub>"), value = 2, min = 0, step = 0.1),
          numericInput("alpha_y", HTML("Skewness parameter &alpha;<sub>Y</sub>"), value = 2, min = 0, step = 0.1),
          sliderInput("B_design", "Monte Carlo size", min = 5000, max = 30000, value = 10000, step = 5000),
          accordion(
            id = "econ_inputs",
            open = FALSE,
            accordion_panel(
              "Advanced economic inputs",
              p(class = "text-muted", "These values are hidden by default and prefilled from the selected benchmark scenario."),
              numericInput("theta_design", HTML("&theta;"), value = 0.01, min = 0.001, step = 0.01),
              numericInput("shift_design", "Shift size", value = 1, min = 0.1, step = 0.1),
              numericInput("i1_design", HTML("i<sub>1</sub>"), value = 150),
              numericInput("i2_design", HTML("i<sub>2</sub>"), value = 50),
              numericInput("eta1_design", HTML("&eta;<sub>1</sub>"), value = 260),
              numericInput("eta2_design", HTML("&eta;<sub>2</sub>"), value = 50),
              numericInput("eta3_design", HTML("&eta;<sub>3,X</sub>"), value = 5),
              numericInput("eta4_design", HTML("&eta;<sub>4,X</sub>"), value = 0.1),
              numericInput("b1_design", HTML("b<sub>1</sub>"), value = 4),
              numericInput("b2_design", HTML("b<sub>2</sub>"), value = 5),
              numericInput("b3_design", HTML("b<sub>3,X</sub>"), value = 0.5)
            )
          ),
          actionButton("run_design", "Run chart design", class = "btn-primary")
        ),
        card(
          card_header("Results"),
          layout_column_wrap(
            width = 1/2,
            card(card_header("Performance summary"), DTOutput("design_metrics_table")),
            card(card_header("Optimal design parameters"), DTOutput("design_parameters_table"))
          ),
          card(card_header("Metric comparison"), plotOutput("design_metric_plot", height = "420px")),
          card(card_header("Interpretation"), htmlOutput("design_interpretation"))
        )
      ), 
      app_footer
    )
  ),
  
  nav_panel(
    "Skewness–Correlation Explorer",
    page_fluid(
      div(
        class = "p-4 mb-4",
        style = "background: white; border-radius: 18px;",
        h3("Impact of skewness parameters on cross-chart correlation"),
        p("Set fixed values for the three remaining inputs and choose one parameter to vary. The plot then scans that selected parameter across the chosen range and shows how the cross-chart correlation changes." )
      ),
      layout_sidebar(
        sidebar = card(
          card_header("Skewness inputs"),
          selectizeInput(
            "vary_param",
            "Parameter to vary",
            choices = names(param_choices),
            selected = "alpha_zeta",
            options = list(
              render = I(sprintf(
                "{ option: function(item, escape) { return '<div>' + %s[item.value] + '</div>'; },
         item: function(item, escape) { return '<div>' + %s[item.value] + '</div>'; } }",
                jsonlite::toJSON(as.list(param_choices), auto_unbox = TRUE),
                jsonlite::toJSON(as.list(param_choices), auto_unbox = TRUE)
              ))
            )
          ),
          numericInput("alpha_zeta", HTML("Fixed &alpha;<sub>&zeta;</sub>"), value = 2, min = -5, max = 5, step = 0.1),
          numericInput("alpha_delta", HTML("Fixed &alpha;<sub>&delta;</sub>"), value = 2, min = -5, max = 5, step = 0.1),
          numericInput("alpha_epsilon", HTML("Fixed &alpha;<sub>&epsilon;</sub>"), value = 2, min = -5, max = 5, step = 0.1),
          sliderInput("beta1_corr", HTML("Fixed &beta;<sub>1</sub>"), min = -1.5, max = 1.5, value = 0.3, step = 0.1),
          fluidRow(
            column(6, numericInput("scan_min", "Scan minimum", value = -5, step = 0.5)),
            column(6, numericInput("scan_max", "Scan maximum", value = 5, step = 0.5))
          ),
          numericInput("scan_step", "Scan step", value = 0.25, min = 0.05, step = 0.05),
          actionButton("run_corr", "Update correlation plot", class = "btn-primary")
        ),
        card(
          card_header("Correlation output"),
          layout_column_wrap(
            width = 1/2,
            value_box(title = "Current cross-chart correlation", value = textOutput("corr_value", inline = TRUE), theme = value_box_theme(bg = "#C41230", fg = "white")),
            card(card_header("Interpretation"), htmlOutput("corr_interpretation"))
          ),
          card(plotOutput("corr_plot", height = "380px"))
        )
      ), 
      app_footer
    )
  ),
  
  nav_panel(
    "Learn",
    page_fluid(
      div(
        class = "p-4",
        style = "background: white; border-radius: 18px;",
        h3("About this app"),
        p("This app helps users explore economically designed two-stage control charts under two modeling frameworks:"),
        tags$ul(
          tags$li("A two-stage skew-normal control chart designed for asymmetric process data, proposed by Su, N. C., Chen, Y. J. & Huang, W. H (2026)."),
          tags$li("A two-stage normal control chart following the framework of Costa, A. F., & De Magalhães, M. S. (2005).")
        ),
        h4("How to use the Design Explorer"),
        tags$ul(
          tags$li("Choose a benchmark scenario from the study or modify the economic inputs manually."),
          tags$li("Select one chart or compare both charts side by side."),
          tags$li("Review the resulting expected net income, detection speed, false alarms, and design parameters.")
        ),
        h4("How to use the Skewness–Correlation Explorer"),
        tags$ul(
          tags$li("Adjust the skewness parameters of the latent components."),
          tags$li("See how those inputs affect the cross-chart correlation studied in the paper."),
          tags$li("Use the scan plot to understand how changes in alpha[zeta] alter the correlation pattern.")
        ),
        h4("Meaning of outputs"),
        tags$ul(
          tags$li(HTML("<b>Expected Net Income (E[A])</b>: higher values indicate better economic performance.")),
          tags$li(HTML("<b>Average Time to Signal (ATS)</b>: smaller values indicate faster detection.")),
          tags$li(HTML("<b>Expected False Alarms (EFA)</b>: smaller values indicate fewer false alarms."))
        ),
        h4("Meaning of key inputs"),
        tags$ul(
          tags$li(HTML("<b>i<sub>1</sub></b>: income per unit time when the process is in control.")),
          tags$li(HTML("<b>i<sub>2</sub></b>: income per unit time when the process is out of control.")),
          tags$li(HTML("<b>&eta;<sub>1</sub></b>: cost of finding and eliminating an assignable cause that occurs once during a cycle.")),
          tags$li(HTML("<b>&eta;<sub>2</sub></b>: cost of investigating a false alarm.")),
          tags$li(HTML("<b>&eta;<sub>3,X</sub></b>: fixed cost of sampling and testing for X variables.")),
          tags$li(HTML("<b>&eta;<sub>4,X</sub></b>: variable cost of sampling and testing for X variables.")),
          tags$li(HTML("<b>1/&theta;</b>: expected interval length for which the process stays in control.")),
          tags$li(HTML("<b>b<sub>1</sub></b>: expected time required to find and eliminate the assignable cause.")),
          tags$li(HTML("<b>b<sub>2</sub></b>: expected time wasted with false alarms.")),
          tags$li(HTML("<b>b<sub>3,X</sub></b>: time required to take and interpret an X sample.")),
          tags$li(HTML("<b>b<sub>3,Y</sub></b>: time required to take and interpret a Y sample. In the benchmark examples, b<sub>3,Y</sub> is set proportionally to b<sub>3,X</sub>.")),
          tags$li(HTML("<b>&eta;<sub>3,Y</sub></b>: fixed cost of sampling and testing for Y variables.")),
          tags$li(HTML("<b>&eta;<sub>4,Y</sub></b>: variable cost of sampling and testing for Y variables.")),
          tags$li(HTML("<b>&beta;<sub>1</sub></b>: linkage parameter connecting the surrogate and primary variables.")),
          tags$li(HTML("<b>&alpha;<sub>&zeta;</sub>, &alpha;<sub>&delta;</sub>, &alpha;<sub>&epsilon;</sub></b>: skewness parameters for the latent process and error components used in the correlation study."))
        )
      ), 
      app_footer
    )
  )
)

# -------------------------------------------------------------------
# Server
# -------------------------------------------------------------------
server <- function(input, output, session) {
  observeEvent(input$scenario, {
    row <- get_scenario_row(as.numeric(input$scenario))
    updateNumericInput(session, "theta_design", value = row$theta)
    updateNumericInput(session, "shift_design", value = row$shift_size)
    updateNumericInput(session, "i1_design", value = row$i1)
    updateNumericInput(session, "i2_design", value = row$i2)
    updateNumericInput(session, "eta1_design", value = row$eta1)
    updateNumericInput(session, "eta2_design", value = row$eta2)
    updateNumericInput(session, "eta3_design", value = row$eta3)
    updateNumericInput(session, "eta4_design", value = row$eta4)
    updateNumericInput(session, "b1_design", value = row$b1)
    updateNumericInput(session, "b2_design", value = row$b2)
    updateNumericInput(session, "b3_design", value = row$b3)
  }, ignoreInit = FALSE)
  
  design_results <- eventReactive(input$run_design, {
    withProgress(message = "Running design comparison", value = 0.2, {
      row <- tibble(
        theta = input$theta_design,
        shift_size = input$shift_design,
        i1 = input$i1_design,
        i2 = input$i2_design,
        eta1 = input$eta1_design,
        eta2 = input$eta2_design,
        eta3 = input$eta3_design,
        eta4 = input$eta4_design,
        b1 = input$b1_design,
        b2 = input$b2_design,
        b3 = input$b3_design
      )
      run_design_comparison(
        row_data = row,
        beta1 = input$beta1,
        alpha_x = input$alpha_x,
        alpha_y = input$alpha_y,
        chart_mode = input$chart_mode,
        B = input$B_design
      )
    })
  })
  
  output$design_metrics_table <- renderDT({
    req(design_results())
    datatable(make_design_metric_table(design_results()), options = list(dom = "t"), rownames = FALSE)
  })
  
  output$design_parameters_table <- renderDT({
    req(design_results())
    datatable(make_design_parameter_table(design_results()), options = list(dom = "t"), rownames = FALSE)
  })
  
  output$design_metric_plot <- renderPlot({
    req(design_results())
    make_design_metric_plot(design_results())
  })
  
  output$design_interpretation <- renderUI({
    req(design_results())
    txt <- if (nrow(design_results()) == 2) {
      "This comparison shows how the assumed distribution affects the resulting economic design. Similar metric values can still arise from different chart parameters and operating trade-offs."
    } else {
      "The displayed results summarize the recommended design and operating characteristics for the selected chart under the chosen economic setting."
    }
    div(style = "font-size: 1rem; line-height: 1.6;", txt)
  })
  
  corr_results <- eventReactive(input$run_corr, {
    validate(need(input$scan_max > input$scan_min, "Scan maximum must be larger than scan minimum."))
    scan_seq <- seq(input$scan_min, input$scan_max, by = input$scan_step)
    current <- compute_cross_chart_correlation_wrapper(
      input$alpha_zeta, input$alpha_delta, input$alpha_epsilon, input$beta1_corr
    )
    grid <- make_correlation_grid(
      vary_param = input$vary_param,
      fixed_alpha_zeta = input$alpha_zeta,
      fixed_alpha_delta = input$alpha_delta,
      fixed_alpha_epsilon = input$alpha_epsilon,
      fixed_beta1 = input$beta1_corr,
      scan_seq = scan_seq
    )
    list(current = current, grid = grid)
  })
  
  output$corr_value <- renderText({
    req(corr_results())
    validate(need(!is.na(corr_results()$current), "NA"))
    round(corr_results()$current, 4)
  })
  
  output$corr_plot <- renderPlot({
    req(corr_results())
    validate(need(all(!is.na(corr_results()$grid$correlation)), "Correlation values could not be computed. Please confirm that rhoxyfun is available in all_functions.R and supports the selected range."))
    
    x_lab <- switch(
      input$vary_param,
      alpha_zeta = expression(alpha[zeta]),
      alpha_delta = expression(alpha[delta]),
      alpha_epsilon = expression(alpha[epsilon]),
      beta1 = expression(beta[1])
    )
    
    plot_title <- switch(
      input$vary_param,
      alpha_zeta = expression(paste("Effect of ", alpha[zeta], " on cross-chart correlation")),
      alpha_delta = expression(paste("Effect of ", alpha[delta], " on cross-chart correlation")),
      alpha_epsilon = expression(paste("Effect of ", alpha[epsilon], " on cross-chart correlation")),
      beta1 = expression(paste("Effect of ", beta[1], " on cross-chart correlation"))
    )
    
    selected_x <- switch(
      input$vary_param,
      alpha_zeta = input$alpha_zeta,
      alpha_delta = input$alpha_delta,
      alpha_epsilon = input$alpha_epsilon,
      beta1 = input$beta1_corr
    )
    
    ggplot(corr_results()$grid, aes(x = scan_value, y = correlation)) +
      geom_line(linewidth = 1.1, color = "#C41230") +
      geom_point(size = 1.5, color = "#2E4057") +
      geom_vline(xintercept = selected_x, linetype = 2, color = "#6B7280") +
      labs(
        x = x_lab,
        y = "Cross-chart correlation",
        title = plot_title
      ) +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold"))
  })
  
  output$corr_interpretation <- renderUI({
    req(corr_results())
    vary_label <- switch(
      input$vary_param,
      alpha_zeta = "αζ",
      alpha_delta = "αδ",
      alpha_epsilon = "αε",
      beta1 = "β1"
    )
    div(
      style = "font-size: 1rem; line-height: 1.6;",
      HTML(paste0(
        "The current cross-chart correlation is <b>", round(corr_results()$current, 4),
        "</b>. The plot varies <b>", vary_label,
        "</b> across the selected scan range while holding the remaining inputs fixed. The dashed vertical line marks the currently selected fixed value."
      ))
    )
  })
}

shinyApp(ui, server)
