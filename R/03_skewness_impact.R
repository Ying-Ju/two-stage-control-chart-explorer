# Load required libraries
pacman::p_load(patchwork, tidyverse, DEoptim, sn)
source("all_functions.R")

df <- data.frame(
  beta1 = rep(c(1, 3, 5, 7), 4),
  alpha_zeta = rep(c(0, 1, 2, 4), each = 4),
  alpha_delta = rep(c(0, 1, 2, 4), each = 4),
  alpha_vareplison = rep(c(0, 1, 2, 4), each = 4)
)

# Use pmap_dfr to apply rhoxyfun row-wise and bind results
df_result <- df %>%
  mutate(row_id = row_number()) %>%
  pmap_dfr(function(beta1, alpha_zeta, alpha_delta, alpha_vareplison, row_id) {
    res <- rhoxyfun(beta1, alpha_zeta, alpha_delta, alpha_vareplison)
    tibble(
      row_id = row_id,
      beta1 = beta1,
      alpha_zeta = alpha_zeta,
      alpha_delta = alpha_delta,
      alpha_vareplison = alpha_vareplison,
      rho = res$rho,
      lambdaX = res$lambdaX,
      lambdaY = res$lambdaY
    )
  })

# Create a label for skewness setting
df_result <- df_result %>%
  mutate(alpha_label = paste0("(", alpha_zeta, ", ", alpha_delta, ", ", alpha_vareplison, ")"))

# FIGURE 1: Line plot of rho vs beta1 for each skewness setting
p_beta1 <- ggplot(df_result, aes(x = beta1, y = rho, color = alpha_label, group = alpha_label)) +
  geom_line() +
  geom_point() +
  labs(
    x = expression(beta[1]),
    y = expression(rho["X,Y"]),
    color = expression(paste("(", alpha[xi], ", ", alpha[delta], ", ", alpha[epsilon], ")"))
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")


df_zeta <- data.frame(
  beta1 = rep(3, 16),
  alpha_zeta = rep(c(0, 1, 2, 4), 4),
  alpha_delta = rep(c(0, 1, 2, 4), each = 4),
  alpha_vareplison = rep(c(0, 1, 2, 4), each = 4)
)

# Use pmap_dfr to apply rhoxyfun row-wise and bind results
df_result_zeta <- df_zeta %>%
  mutate(row_id = row_number()) %>%
  pmap_dfr(function(beta1, alpha_zeta, alpha_delta, alpha_vareplison, row_id) {
    res <- rhoxyfun(beta1, alpha_zeta, alpha_delta, alpha_vareplison)
    tibble(
      row_id = row_id,
      beta1 = beta1,
      alpha_zeta = alpha_zeta,
      alpha_delta = alpha_delta,
      alpha_vareplison = alpha_vareplison,
      rho = res$rho,
      lambdaX = res$lambdaX,
      lambdaY = res$lambdaY
    )
  })

# Create a label for skewness setting
df_result_zeta <- df_result_zeta %>%
  mutate(alpha_label = paste0("(", alpha_delta, ", ", alpha_vareplison, ")"))

p_alphaxi <- ggplot(df_result_zeta, aes(x = alpha_zeta, y = rho, 
                                  color = alpha_label, 
                                  group = alpha_label)) +
  geom_line() +
  geom_point() +
  ylim(c(0.35, 0.85)) +
  labs(
    x = expression(alpha[xi]),
    y = expression(rho["X,Y"]),
    color = expression(paste("(", alpha[delta], ", ", alpha[epsilon], ")"))
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

p_alphaxi

df_delta <- data.frame(
  beta1 = rep(3, 16),
  alpha_zeta = rep(c(0, 1, 2, 4), each = 4),
  alpha_delta = rep(c(0, 1, 2, 4), 4),
  alpha_vareplison = rep(c(0, 1, 2, 4), each = 4)
)

# Use pmap_dfr to apply rhoxyfun row-wise and bind results
df_result_delta <- df_delta %>%
  mutate(row_id = row_number()) %>%
  pmap_dfr(function(beta1, alpha_zeta, alpha_delta, alpha_vareplison, row_id) {
    res <- rhoxyfun(beta1, alpha_zeta, alpha_delta, alpha_vareplison)
    tibble(
      row_id = row_id,
      beta1 = beta1,
      alpha_zeta = alpha_zeta,
      alpha_delta = alpha_delta,
      alpha_vareplison = alpha_vareplison,
      rho = res$rho,
      lambdaX = res$lambdaX,
      lambdaY = res$lambdaY
    )
  })

# Create a label for skewness setting
df_result_delta <- df_result_delta %>%
  mutate(alpha_label = paste0("(", alpha_zeta, ", ", alpha_vareplison, ")"))

p_alphadelta <- ggplot(df_result_delta, aes(x = alpha_delta, y = rho, 
                                            color = alpha_label, 
                                            group = alpha_label)) +
  geom_line() +
  geom_point() +
  ylim(c(0.35, 0.85)) +
  labs(
    x = expression(alpha[delta]),
    y = expression(rho["X,Y"]),
    color = expression(paste("(", alpha[xi], ", ", alpha[epsilon], ")"))
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

p_alphadelta

df_epsilon <- data.frame(
  beta1 = rep(3, 16),
  alpha_zeta = rep(c(0, 1, 2, 4), each = 4),
  alpha_delta = rep(c(0, 1, 2, 4), each = 4),
  alpha_vareplison = rep(c(0, 1, 2, 4), 4)
)

# Use pmap_dfr to apply rhoxyfun row-wise and bind results
df_result_epsilon <- df_epsilon %>%
  mutate(row_id = row_number()) %>%
  pmap_dfr(function(beta1, alpha_zeta, alpha_delta, alpha_vareplison, row_id) {
    res <- rhoxyfun(beta1, alpha_zeta, alpha_delta, alpha_vareplison)
    tibble(
      row_id = row_id,
      beta1 = beta1,
      alpha_zeta = alpha_zeta,
      alpha_delta = alpha_delta,
      alpha_vareplison = alpha_vareplison,
      rho = res$rho,
      lambdaX = res$lambdaX,
      lambdaY = res$lambdaY
    )
  })

# Create a label for skewness setting
df_result_epsilon <- df_result_epsilon %>%
  mutate(alpha_label = paste0("(", alpha_zeta, ", ", alpha_delta, ")"))

p_alphaepsilon <- ggplot(df_result_epsilon, aes(x = alpha_vareplison, y = rho, 
                                                color = alpha_label, 
                                                group = alpha_label)) +
  geom_line() +
  geom_point() +
  ylim(c(0.35, 0.85)) +
  labs(
    x = expression(alpha[epsilon]),
    y = expression(rho["X,Y"]),
    color = expression(paste("(", alpha[xi], ", ", alpha[delta], ")"))
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

p_alphaepsilon

# Combine plots in a 2x2 layout
combined_plot <- (p_beta1 | p_alphaxi) /
  (p_alphadelta | p_alphaepsilon)

combined_plot

#ggsave("figures/sim_pars.pdf", width=9, height = 5.5, units = "in")

