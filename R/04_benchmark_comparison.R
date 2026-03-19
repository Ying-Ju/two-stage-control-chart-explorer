pacman::p_load(patchwork, tidyverse, DEoptim, sn)

source("all_functions.R")

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
  ncol = 12, nrow=16, byrow=T)

income_df <- data.frame(income_df)
colnames(income_df) <- c("example", "theta", "shift_size",
                         "i1", "i2",
                         "eta1", "eta2", "eta3", "eta4",
                         "b1", "b2", "b3")

# ------------------------------------------------------------
# select Example 10 and Example 14
# ------------------------------------------------------------
ex_compare <- income_df %>%
  filter(example %in% c(10, 14))

# ------------------------------------------------------------
# run comparison for each example
# ------------------------------------------------------------
comparison_results <- map_dfr(seq_len(nrow(ex_compare)), function(i) {
  
  row <- ex_compare[i, ]
  
  out <- run_skew_vs_normal_comparison(
    theta      = row$theta,
    shift_size = row$shift_size,
    b1         = row$b1,
    b2         = row$b2,
    b3x        = row$b3,
    i1         = row$i1,
    i2         = row$i2,
    eta1       = row$eta1,
    eta2       = row$eta2,
    eta3x      = row$eta3,
    eta4x      = row$eta4,
    alpha_x    = 2,
    alpha_y    = 2,
    beta1      = 0.3,
    sigma_zeta = 1,
    sigma_x0   = 1,
    sigma_y0   = 1,
    B          = 100000,
    seed_x     = 123,
    seed_y     = 456
  )
  
  bind_cols(
    tibble(
      Example    = row$example,
      alpha_x    = 2,
      alpha_y    = 2,
      beta1      = 3
    ),
    out
  )
})

comparison_results


#--------------------------------------------------
# Prepare plotting data
#--------------------------------------------------
plot_df <- comparison_results %>%
  transmute(
    Example = factor(paste0("Ex.", Example), levels = c("Ex.10", "Ex.14")),
    Normal_EA   = EA_norm,
    Skew_EA     = EA_skew,
    Normal_ATS  = ATS_norm,
    Skew_ATS    = ATS_skew,
    Normal_EFA  = EFA_norm,
    Skew_EFA    = EFA_skew
  )

ea_df <- plot_df %>%
  select(Example, Normal_EA, Skew_EA) %>%
  pivot_longer(
    cols = -Example,
    names_to = "Distribution",
    values_to = "Value"
  ) %>%
  mutate(
    Distribution = factor(
      ifelse(grepl("^Normal", Distribution), "Normal", "Skew-Normal"),
      levels = c("Normal", "Skew-Normal")
    )
  )

ats_df <- plot_df %>%
  select(Example, Normal_ATS, Skew_ATS) %>%
  pivot_longer(
    cols = -Example,
    names_to = "Distribution",
    values_to = "Value"
  ) %>%
  mutate(
    Distribution = factor(
      ifelse(grepl("^Normal", Distribution), "Normal", "Skew-Normal"),
      levels = c("Normal", "Skew-Normal")
    )
  )

efa_df <- plot_df %>%
  select(Example, Normal_EFA, Skew_EFA) %>%
  pivot_longer(
    cols = -Example,
    names_to = "Distribution",
    values_to = "Value"
  ) %>%
  mutate(
    Distribution = factor(
      ifelse(grepl("^Normal", Distribution), "Normal", "Skew-Normal"),
      levels = c("Normal", "Skew-Normal")
    )
  )

#--------------------------------------------------
# Common theme
#--------------------------------------------------

common_theme <- theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"))


fill_scale <- scale_fill_manual(
  values = c("Normal" = "#E07A6A", "Skew-Normal" = "#67B7BE")
)

#--------------------------------------------------
# Panel 1: Economic Performance
#--------------------------------------------------
p1 <- ggplot(ea_df, aes(x = Example, y = Value, fill = Distribution)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65) +
  labs(
    title = "Economic Performance",
    y = "Expected Net Income (E[A])",
    fill = "Distribution"
  ) +
  fill_scale +
  common_theme

#--------------------------------------------------
# Panel 2: Detection Speed
#--------------------------------------------------
p2 <- ggplot(ats_df, aes(x = Example, y = Value, fill = Distribution)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65) +
  labs(
    title = "Detection Speed",
    y = "Average Time to Signal (ATS)",
    fill = "Distribution"
  ) +
  fill_scale +
  common_theme

#--------------------------------------------------
# Panel 3: False Alarm Rate
#--------------------------------------------------
p3 <- ggplot(efa_df, aes(x = Example, y = Value, fill = Distribution)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65) +
  labs(
    title = "False Alarm Rate",
    y = "Expected False Alarms (EFA)",
    fill = "Distribution"
  ) +
  fill_scale +
  common_theme

#--------------------------------------------------
# Combine panels
#--------------------------------------------------
final_plot <- (p1 + p2 + p3) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

final_plot
ggsave("figures/benchmark_comparison.pdf", final_plot, width=9, height = 3.5, units = "in")

