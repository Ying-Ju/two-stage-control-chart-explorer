pacman::p_load(patchwork, tidyverse, DEoptim, sn)
source("00_all_functions.R")

res_sim <- exp_income_skew(
  nx = 5, ny = 3,
  hx = 1.0, hy = 0.5,
  kx = 1, ky = 2, wx = 0.8,
  beta1 = 0.6, theta = 0.01, shift_size = 2,
  b1 = 4, b2 = 40, b3x = 0.05,
  i1 = 150, i2 = 50,
  eta1 = 260, eta2 = 500,
  eta3x = 5, eta4x = 0.1,
  alpha_x = 2, alpha_y = 2,
  sigma_zeta = 1, sigma_x0 = 1, sigma_y0 = 1,
  B = 100000
)

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



# Create a result list to hold outputs
all_results <- vector("list", nrow(income_df))

set.seed(2025)

# Loop over each case
for (i in 1:nrow(income_df)) {
  row <- income_df[i, ]
  
  cat("Running example:", row$example, "\n")
  
  res <- maximize_exp_income_skew(
    beta1 = 0.9,
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
    eta4x = row$eta4,
    alpha_x = 2, alpha_y = 2,
    sigma_zeta = 1,
    sigma_x0 = 1,
    sigma_y0 = 1,
    B = 100000,
    seed_x = 123,
    seed_y = 456
  )
  
  all_results[[i]] <- list(
    example = row$example,
    EA = res$EA,
    ATS = res$details$ATS,
    opt_par = res$opt_par
  )
}

# Convert to a data frame for summary
results_df <- bind_rows(lapply(all_results, function(x) {
  tibble(
    example = x$example,
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
print(xtable::xtable(results_df, type = "latex", digits=3), include.rownames = FALSE)
