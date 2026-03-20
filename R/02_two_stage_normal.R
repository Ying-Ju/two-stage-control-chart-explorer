pacman::p_load(patchwork, tidyverse, DEoptim)

source("00_all_functions.R")

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

costa_df <- tibble::tribble(
  ~example, ~beta1, ~nx, ~ny, ~hx,  ~hy,  ~kx,  ~ky,  ~wx,  ~EA_costa,
  2,       0.3,    15,   1,  6.20, 1.43, 2.86, 0.01, 1.13, 134.21,
  2,       0.6,     9,  27,  0.25, 2.45, 2.65, 2.20, 1.02, 137.84,
  2,       0.9,     1,  28,  0.09, 2.55, 0.01, 3.56, 0.01, 138.98,
  
  4,       0.3,     7,   1,  1.69, 0.19, 4.00, 0.01, 0.01, 140.80,
  4,       0.6,     3,   9,  0.05, 0.51, 3.73, 2.58, 1.09, 141.97,
  4,       0.9,     2,   8,  0.03, 0.50, 2.87, 3.74, 0.58, 142.16,
  
  6,       0.3,    11,   1,  2.23, 0.83, 2.67, 0.01, 1.19,  14.36,
  6,       0.6,     1,  45,  0.04, 1.53, 0.01, 3.20, 0.01,  18.88,
  6,       0.9,     1,  24,  0.04, 1.05, 0.01, 3.49, 0.01,  21.40,
  
  8,       0.3,     5,   1,  1.58, 0.24, 2.97, 0.01, 0.01,  30.00,
  8,       0.6,     1,  17,  0.06, 0.54, 0.01, 3.59, 0.01,  33.92,
  8,       0.9,     1,  10,  0.05, 0.52, 0.01, 3.85, 0.01,  34.50,
  
  10,       0.3,     8,   1,  2.46, 2.34, 2.75, 0.01, 1.02, 132.40,
  10,       0.6,     1,  28,  0.18, 0.81, 0.01, 3.46, 0.01, 136.00,
  10,       0.9,     1,  18,  0.18, 0.92, 0.01, 3.61, 0.01, 137.69,
  
  12,       0.3,     4,   1,  2.78, 1.03, 3.57, 0.01, 1.51, 138.72,
  12,       0.6,     2,   5,  0.10, 0.87, 3.41, 2.25, 0.86, 141.63,
  12,       0.9,     1,   5,  0.05, 0.83, 2.58, 3.46, 0.31, 142.42,
  
  14,       0.3,     5,   1,  0.34, 0.94, 3.06, 0.01, 0.74,  14.98,
  14,       0.6,     1,  19,  0.07, 0.13, 0.01, 3.77, 0.01,  18.74,
  14,       0.9,     1,  12,  0.06, 0.14, 0.01, 3.94, 0.01,  22.79,
  
  16,       0.3,     2,   3,  1.19, 1.26, 2.68, 0.01, 1.02,  23.99,
  16,       0.6,     1,   7,  0.19, 0.56, 1.68, 2.88, 0.60,  29.43,
  16,       0.9,     1,   5,  0.19, 0.61, 0.01, 3.50, 0.01,  31.74,
  
  18,       0.3,    17,   1,  6.80, 1.43, 3.07, 0.01, 0.01,  34.15,
  18,       0.6,    10,  26,  0.37, 2.41, 2.80, 2.20, 1.13,  37.34,
  18,       0.9,     1,  30,  0.09, 2.62, 0.01, 3.73, 0.01,  38.42,
  
  20,       0.3,     7,   2,  1.60, 0.12, 3.83, 0.01, 0.01,  37.56,
  20,       0.6,     3,   9,  0.05, 0.53, 3.35, 2.46, 1.14,  38.41,
  20,       0.9,     1,   9,  0.03, 0.59, 0.01, 4.00, 0.01,  38.54
)

EA_input_df <- costa_df %>%
  left_join(
    income_df %>% select(-any_of("beta1")),
    by = "example"
  )

EA_compare_df <- EA_input_df %>%
  rowwise() %>%
  mutate(
    EA_R = exp_income_normal(
      nx = nx, ny = ny, hx = hx, hy = hy,
      kx = kx, ky = ky, wx = wx,
      beta1 = beta1,
      theta = theta,
      shift_size = shift_size,
      b1 = b1, b2 = b2, b3x = b3,
      i1 = i1, i2 = i2,
      eta1 = eta1, eta2 = eta2,
      eta3x = eta3, eta4x = eta4
    )$EA
  ) %>%
  ungroup() %>%
  mutate(
    abs_diff = EA_R - EA_costa,
    rel_diff_pct = 100 * abs_diff / EA_costa
  )

EA_compare_df %>%
  transmute(
    example,
    beta1,
    EA_costa = round(EA_costa, 2),
    EA_R = round(EA_R, 6),
    abs_diff = round(abs_diff, 6),
    rel_diff_pct = round(rel_diff_pct, 6)
  )

EA_compare_df %>%
  summarise(
    max_abs_diff = max(abs(abs_diff), na.rm = TRUE),
    mean_abs_diff = mean(abs(abs_diff), na.rm = TRUE),
    max_rel_diff_pct = max(abs(rel_diff_pct), na.rm = TRUE),
    mean_rel_diff_pct = mean(abs(rel_diff_pct), na.rm = TRUE)
  )

EA_compare_df %>%
  group_by(example) %>%
  summarise(
    max_abs_diff = max(abs(abs_diff)),
    max_rel_diff_pct = max(abs(rel_diff_pct))
  ) %>%
  arrange(example)


beta_levels <- c(0.3, 0.6, 0.9)


income_df_expanded <- income_df %>%
  tidyr::crossing(beta1 = beta_levels)

# Create a result list to hold outputs
all_results <- vector("list", nrow(income_df_expanded))

set.seed(2025)

# Loop over each case
for (i in 1:nrow(income_df_expanded)) {
  row <- income_df_expanded[i, ]
  
  cat("Running example:", row$example, "\n")
  
  res <- maximize_exp_income_normal(
    beta1 = row$beta1,
    theta = row$theta,
    shift_size = row$shift_size,
    b1 = row$b1,
    b2 = row$b2,
    b3x = row$b3,
    i1 = row$i1,
    i2 = row$i2,
    eta1 = row$eta1,
    eta2 = row$eta2,
    eta3x = row$eta3,
    eta4x = row$eta4
    #alpha_x = 2, alpha_y = 2
  )
  
  
  all_results[[i]] <- list(
    example = row$example,
    beta1 = row$beta1,
    EA = res$EA,
    ATS = res$details$ATS,
    opt_par = res$opt_par
  )
}

# Convert to a data frame for summary
results_df <- bind_rows(lapply(all_results, function(x) {
  tibble(
    example = x$example,
    beta1 = x$beta1,
    EA = x$EA,
    ATS = x$ATS,
    nx = x$opt_par[1],
    ny = x$opt_par[2],
    hx = x$opt_par[3],
    hy = x$opt_par[4],
    kx = x$opt_par[5],
    ky = x$opt_par[6],
    wx = x$opt_par[7]
  )
}))

results_df |> view()
results_df$nx <- as.integer(results_df$nx)
results_df$ny <- as.integer(results_df$ny)
results_df$example <- as.integer(results_df$example)

results_df_beta <- results_df |> 
  filter(beta1 == 0.9) |> 
  select(-beta1)

print(xtable::xtable(results_df_beta, type = "latex", digits=3), include.rownames = FALSE)

