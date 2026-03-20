rhoxyfun <- function(beta1, alphazeta, alphadelta, alphaepsilon,
                     sigmazetasq = 0.5, sigmadeltasq = 0.5, sigmaepsilonsq = 0.5) {
  
  # LambdaX
  lambdaX <- (
    alphazeta * sqrt(sigmazetasq) + alphadelta * sqrt(sigmadeltasq)
  ) / sqrt(
    (alphazeta * sqrt(sigmadeltasq) - alphadelta * sqrt(sigmazetasq))^2 +
      (sigmazetasq + sigmadeltasq) * (alphaepsilon^2 + 1)
  )
  
  # LambdaY
  lambdaY <- (
    beta1 * alphazeta * sqrt(sigmazetasq) + alphaepsilon * sqrt(sigmaepsilonsq)
  ) / sqrt(
    (beta1 * alphaepsilon * sqrt(sigmazetasq) - alphazeta * sqrt(sigmaepsilonsq))^2 +
      (beta1^2 * sigmazetasq + sigmaepsilonsq) * (alphadelta^2 + 1)
  )
  
  # Covariance
  covxy <- (beta1 * sigmazetasq) -
    ((2 / pi) *
       ((alphazeta * sqrt(sigmazetasq) + alphadelta * sqrt(sigmadeltasq)) *
          (alphaepsilon * sqrt(sigmaepsilonsq) + beta1 * alphazeta * sqrt(sigmazetasq))) /
       (alphazeta^2 + alphadelta^2 + alphaepsilon^2 + 1))
  
  # Variances
  varx <- sqrt(
    (sigmazetasq + sigmadeltasq) -
      ((2 / pi) *
         ((alphazeta * sqrt(sigmazetasq) + alphadelta * sqrt(sigmadeltasq))^2) /
         (alphazeta^2 + alphadelta^2 + alphaepsilon^2 + 1))
  )
  
  vary <- sqrt(
    (beta1^2 * sigmazetasq + sigmaepsilonsq) -
      ((2 / pi) *
         ((beta1 * alphazeta * sqrt(sigmazetasq) + alphaepsilon * sqrt(sigmaepsilonsq))^2) /
         (alphazeta^2 + alphadelta^2 + alphaepsilon^2 + 1))
  )
  
  # Correlation
  rhoxy <- covxy / (varx * vary)
  
  return(list(rho = rhoxy, lambdaX = lambdaX, lambdaY = lambdaY))
}



# --- Helper probability functions ---
# P(|N(mu,1)| < a)
p_abs_less <- function(a, mu = 0) {
  pnorm(a - mu) - pnorm(-a - mu)
}

# P(|N(mu,1)| > a)
p_abs_greater <- function(a, mu = 0) {
  1 - p_abs_less(a, mu)
}

# P(w < |N(mu,1)| < k)
p_abs_between <- function(w, k, mu = 0) {
  p_abs_less(k, mu) - p_abs_less(w, mu)
}


exp_income_normal <- function(nx, ny, hx, hy, kx, ky, wx, 
                              beta1, theta, shift_size, b1, b2, b3x,
                              i1, i2, 
                              eta1, eta2, eta3x, eta4x){
  
  b3y <- 0.2*b3x
  eta3y <- 0.1*eta3x
  eta4y <- 0.1*eta4x
  bT <- c(exp(-theta*hy), 0, (1 - exp(-theta*hy)), 0)
  # --- Means of shifted standardized statistics ---
  mux <- shift_size * sqrt(nx)            # mean of U_x
  muy <- beta1 * shift_size * sqrt(ny)    # mean of U_y
  
  # --- In-control probabilities ---
  pZ_less_ky    <- p_abs_less(ky, mu = 0)
  pZ_greater_ky <- p_abs_greater(ky, mu = 0)
  pZ_between    <- p_abs_between(wx, kx, mu = 0)
  
  # --- Out-of-control probabilities ---
  pUy_less_ky <- p_abs_less(ky, mu = muy)
  pUx_less_wx <- p_abs_less(wx, mu = mux)
  pUx_between <- p_abs_between(wx, kx, mu = mux)
  
  # --- Out-of-control probabilities ---
  pUy_less_ky <- p_abs_less(ky, mu = muy)
  pUx_less_wx <- p_abs_less(wx, mu = mux)
  pUx_between <- p_abs_between(wx, kx, mu = mux)
  
  # --- Transition probabilities ---
  p11 <- pZ_less_ky * exp(-theta * hy)
  p12 <- pZ_greater_ky * exp(-theta * hx) 
  p13 <- pZ_less_ky * (1 - exp(-theta * hy))
  p14 <- pZ_greater_ky * (1 - exp(-theta * hx))
  
  p21 <- (1 - pZ_between) * exp(-theta * hy)
  p22 <- pZ_between * exp(-theta * hx)
  p23 <- (1 - pZ_between) * (1 - exp(-theta * hy))
  p24 <- pZ_between * (1 - exp(-theta * hx))
  
  p33 <- pUy_less_ky
  p34 <- 1 - p33
  
  p43 <- pUx_less_wx
  p44 <- pUx_between
  
  Q <- matrix(c(p11,p12,p13,p14,
                p21,p22,p23,p24,
                0,0,p33,p34,
                0,0,p43,p44),4,4,byrow=T)
  
  IQT <- solve(diag(4)-Q)
  mv <- bT %*% IQT
  ATS <- bT %*% IQT %*% c(hy,hx,hy,hx)
  my0 <- mv[1,1]; mx0=mv[1,2];
  my1 <- mv[1,3]; mx1=mv[1,4];
  
  EFA <- mx0 * (2 * (1 - pnorm(kx)))
  
  ET <- ATS + (b3x*nx) + (b3y*ny) + (b2*EFA) + b1
  
  EIv <- i1/theta +
    i2 * (ATS - 1/theta + b3x*nx + b3y*ny) -
    eta1 - eta2*EFA -
    (eta3x + (eta4x*nx)) * (mx0 + mx1) -
    (eta3y + (eta4y*ny)) * (my0 + my1)
  
  EA <- EIv/ET
  return(list(EA = EA, EIv = EIv, ATS = ATS, ET = ET, EFA = EFA))
}



maximize_exp_income_normal <- function(beta1, theta, shift_size, b1, b2, b3x,
                                       i1, i2, eta1, eta2, eta3x, eta4x) {
  
  objective <- function(par) {
    nx <- max(1, round(par[1]))
    ny <- max(1, round(par[2]))
    hx <- par[3]
    hy <- par[4]
    kx <- par[5]
    ky <- par[6]
    wx <- par[7]
    
    # Feasibility checks
    if (wx <= 0 || wx >= kx ||
        b3x * nx > hx ||
        (0.2 * b3x * ny) > hy ||
        hx < 0.01 || hx > 20 ||
        hy < 0.01 || hy > 20 ||
        kx < 0.01 || kx > 4 ||
        ky < 0.01 || ky > 4) {
      return(Inf)
    }
    
    val <- tryCatch(
      exp_income_normal(nx, ny, hx, hy, kx, ky, wx,
                        beta1, theta, shift_size,
                        b1, b2, b3x, i1, i2,
                        eta1, eta2, eta3x, eta4x),
      error = function(e) NULL
    )
    
    if (is.null(val) || !is.finite(val$EA)) return(Inf)
    
    return(-as.numeric(val$EA))
  }
  
  lower <- c(1, 1, 0.01, 0.01, 0.01, 0.01, 0.01)
  upper <- c(100, 100, 20, 20, 4, 4, 4)
  
  set.seed(2025)
  result <- DEoptim(
    fn = objective,
    lower = lower,
    upper = upper,
    control = DEoptim.control(trace = TRUE, itermax = 300)
  )
  
  best <- result$optim$bestmem
  best[1] <- round(best[1])
  best[2] <- round(best[2])
  
  final <- exp_income_normal(best[1], best[2], best[3], best[4],
                             best[5], best[6], best[7],
                             beta1, theta, shift_size, b1, b2, b3x,
                             i1, i2, eta1, eta2, eta3x, eta4x)
  
  list(opt_par = best, EA = final$EA, details = final, raw = result)
}


# Functions for skew-normal control limit adjustments
gamma1 <- function(alpha) {
  1 / sqrt(1 - (2 / pi) * (alpha^2 / (1 + alpha^2)))
}

gamma2 <- function(alpha) {
  g1 <- gamma1(alpha)
  g1 * sqrt(2 / pi) * (alpha / sqrt(1 + alpha^2))
}

# Cache environment
.zbar_cache <- new.env(parent = emptyenv())

.make_cache_key <- function(n, alpha, B, seed) {
  paste0("n=", n, "_alpha=", format(alpha, digits = 12),
         "_B=", B, "_seed=", seed)
}

simulate_zbar <- function(n, alpha, B = 20000, seed = 123) {
  key <- .make_cache_key(n, alpha, B, seed)
  
  if (exists(key, envir = .zbar_cache, inherits = FALSE)) {
    return(get(key, envir = .zbar_cache, inherits = FALSE))
  }
  
  old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (old_seed_exists) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  
  set.seed(seed)
  zbar <- replicate(B, mean(sn::rsn(n, xi = 0, omega = 1, alpha = alpha)))
  zbar <- sort(as.numeric(zbar))
  
  assign(key, zbar, envir = .zbar_cache)
  
  if (old_seed_exists) {
    assign(".Random.seed", old_seed, envir = .GlobalEnv)
  }
  
  zbar
}

q_zbar_sim <- function(p, n, alpha, B = 20000, seed = 123, type = 8) {
  zbar <- simulate_zbar(n, alpha, B = B, seed = seed)
  as.numeric(stats::quantile(zbar, probs = p, type = type, names = FALSE))
}

p_zbar_between_sim <- function(lower, upper, n, alpha, B = 20000, seed = 123) {
  zbar <- simulate_zbar(n, alpha, B = B, seed = seed)
  pr <- mean(zbar > lower & zbar < upper)
  max(min(as.numeric(pr), 1), 0)
}

exp_income_skew <- function(nx, ny, hx, hy, kx, ky, wx,
                            beta1, theta, shift_size,
                            b1, b2, b3x,
                            i1, i2,
                            eta1, eta2, eta3x, eta4x,
                            alpha_x = 2, alpha_y = 2,
                            sigma_zeta = 1,
                            sigma_x0 = 1,
                            sigma_y0 = 1,
                            B = 100000,
                            seed_x = 123,
                            seed_y = 456) {
  
  b3y   <- 0.2 * b3x
  eta3y <- 0.1 * eta3x
  eta4y <- 0.1 * eta4x
  
  # Convert Costa-style (kx, ky, wx) to tail probabilities
  px <- 2 * (1 - pnorm(kx))
  py <- 2 * (1 - pnorm(ky))
  pw <- 2 * (1 - pnorm(wx))
  
  # Standardized shifts from manuscript
  delta_x <- shift_size * sigma_zeta / (gamma1(alpha_x) * sigma_x0)
  delta_y <- beta1 * shift_size * sigma_zeta / (gamma1(alpha_y) * sigma_y0)
  
  phys <- exp(-theta * hy)
  phy  <- 1 - phys
  phxs <- exp(-theta * hx)
  phx  <- 1 - phxs
  
  bT <- c(phys, 0, phy, 0)
  
  # Quantiles of simulated sample-mean distributions
  qY_L <- q_zbar_sim(py / 2,     ny, alpha_y, B = B, seed = seed_y)
  qY_U <- q_zbar_sim(1 - py / 2, ny, alpha_y, B = B, seed = seed_y)
  
  qX_L <- q_zbar_sim(px / 2,     nx, alpha_x, B = B, seed = seed_x)
  qX_U <- q_zbar_sim(1 - px / 2, nx, alpha_x, B = B, seed = seed_x)
  
  qW_L <- q_zbar_sim(pw / 2,     nx, alpha_x, B = B, seed = seed_x)
  qW_U <- q_zbar_sim(1 - pw / 2, nx, alpha_x, B = B, seed = seed_x)
  
  # In-control probabilities
  pY_in_0 <- p_zbar_between_sim(qY_L, qY_U, ny, alpha_y, B = B, seed = seed_y)
  
  pX_between_0 <-
    p_zbar_between_sim(qX_L, qW_L, nx, alpha_x, B = B, seed = seed_x) +
    p_zbar_between_sim(qW_U, qX_U, nx, alpha_x, B = B, seed = seed_x)
  
  # Out-of-control probabilities
  pY_in_1 <- p_zbar_between_sim(qY_L - delta_y, qY_U - delta_y,
                                ny, alpha_y, B = B, seed = seed_y)
  
  pX_warn_1 <- p_zbar_between_sim(qW_L - delta_x, qW_U - delta_x,
                                  nx, alpha_x, B = B, seed = seed_x)
  
  pX_between_1 <-
    p_zbar_between_sim(qX_L - delta_x, qW_L - delta_x,
                       nx, alpha_x, B = B, seed = seed_x) +
    p_zbar_between_sim(qW_U - delta_x, qX_U - delta_x,
                       nx, alpha_x, B = B, seed = seed_x)
  
  # Transition probabilities
  p11 <- pY_in_0 * phys
  p12 <- (1 - pY_in_0) * phxs
  p13 <- pY_in_0 * phy
  p14 <- (1 - pY_in_0) * phx
  
  p21 <- (1 - pX_between_0) * phys
  p22 <- pX_between_0 * phxs
  p23 <- (1 - pX_between_0) * phy
  p24 <- pX_between_0 * phx
  
  p33 <- pY_in_1
  p34 <- 1 - p33
  
  p43 <- pX_warn_1
  p44 <- pX_between_1
  
  Q <- matrix(c(
    p11, p12, p13, p14,
    p21, p22, p23, p24,
    0,   0,   p33, p34,
    0,   0,   p43, p44
  ), 4, 4, byrow = TRUE)
  
  IQT <- solve(diag(4) - Q)
  mv  <- bT %*% IQT
  ATS <- as.numeric(bT %*% IQT %*% c(hy, hx, hy, hx))
  
  my0 <- mv[1, 1]
  mx0 <- mv[1, 2]
  my1 <- mv[1, 3]
  mx1 <- mv[1, 4]
  
  EFA <- mx0 * px
  
  ET <- ATS + b3x * nx + b3y * ny + b2 * EFA + b1
  
  EIv <- i1 / theta +
    i2 * (ATS - 1 / theta + b3x * nx + b3y * ny) -
    eta1 - eta2 * EFA -
    (eta3x + eta4x * nx) * (mx0 + mx1) -
    (eta3y + eta4y * ny) * (my0 + my1)
  
  EA <- EIv / ET
  
  list(
    EA = EA,
    EIv = EIv,
    ATS = ATS,
    ET = ET,
    EFA = EFA,
    px = px, py = py, pw = pw,
    Q = Q
  )
}

maximize_exp_income_skew <- function(beta1, theta, shift_size, b1, b2, b3x,
                                     i1, i2, eta1, eta2, eta3x, eta4x,
                                     alpha_x = 2, alpha_y = 2,
                                     sigma_zeta = 1,
                                     sigma_x0 = 1,
                                     sigma_y0 = 1,
                                     B = 100000,
                                     seed_x = 123,
                                     seed_y = 456) {
  
  objective <- function(par) {
    nx <- max(1, round(par[1]))
    ny <- max(1, round(par[2]))
    hx <- par[3]
    hy <- par[4]
    kx <- par[5]
    ky <- par[6]
    wx <- par[7]
    
    if (wx <= 0 || wx >= kx ||
        b3x * nx > hx ||
        (0.2 * b3x * ny) > hy ||
        hx < 0.01 || hx > 20 ||
        hy < 0.01 || hy > 20 ||
        kx < 0.01 || kx > 4 ||
        ky < 0.01 || ky > 4) {
      return(Inf)
    }
    
    val <- tryCatch(
      exp_income_skew(
        nx, ny, hx, hy, kx, ky, wx,
        beta1, theta, shift_size,
        b1, b2, b3x,
        i1, i2, eta1, eta2, eta3x, eta4x,
        alpha_x = alpha_x, alpha_y = alpha_y,
        sigma_zeta = sigma_zeta,
        sigma_x0 = sigma_x0,
        sigma_y0 = sigma_y0,
        B = B,
        seed_x = seed_x,
        seed_y = seed_y
      ),
      error = function(e) NULL
    )
    
    if (is.null(val) || !is.finite(val$EA)) return(Inf)
    -as.numeric(val$EA)
  }
  
  lower <- c(1, 1, 0.01, 0.01, 0.01, 0.01, 0.01)
  upper <- c(100, 100, 20, 20, 4, 4, 4)
  
  set.seed(2025)
  result <- DEoptim(
    fn = objective,
    lower = lower,
    upper = upper,
    control = DEoptim.control(trace = TRUE, itermax = 300)
  )
  
  best <- result$optim$bestmem
  best[1:2] <- round(best[1:2])
  
  final <- exp_income_skew(
    best[1], best[2], best[3], best[4], best[5], best[6], best[7],
    beta1, theta, shift_size,
    b1, b2, b3x,
    i1, i2, eta1, eta2, eta3x, eta4x,
    alpha_x = alpha_x, alpha_y = alpha_y,
    sigma_zeta = sigma_zeta,
    sigma_x0 = sigma_x0,
    sigma_y0 = sigma_y0,
    B = B,
    seed_x = seed_x,
    seed_y = seed_y
  )
  
  list(opt_par = best, EA = final$EA, details = final, raw = result)
}

# ----------------------------
# skew data + normal chart
# ----------------------------

exp_income_skewdata_normalchart <- function(nx, ny, hx, hy, kx, ky, wx,
                                            beta1, theta, shift_size,
                                            b1, b2, b3x,
                                            i1, i2,
                                            eta1, eta2, eta3x, eta4x,
                                            alpha_x = 2, alpha_y = 2,
                                            sigma_zeta = 1,
                                            sigma_x0 = 1,
                                            sigma_y0 = 1,
                                            B = 20000,
                                            seed_x = 123,
                                            seed_y = 456) {
  
  b3y   <- 0.2 * b3x
  eta3y <- 0.1 * eta3x
  eta4y <- 0.1 * eta4x
  
  # standardized shifts from manuscript
  delta_x <- shift_size * sigma_zeta / (gamma1(alpha_x) * sigma_x0)
  delta_y <- beta1 * shift_size * sigma_zeta / (gamma1(alpha_y) * sigma_y0)
  
  # Costa-style starting vector
  phys <- exp(-theta * hy)
  phy  <- 1 - phys
  phxs <- exp(-theta * hx)
  phx  <- 1 - phxs
  bT <- c(phys, 0, phy, 0)
  
  # normal-chart limits on Zbar scale
  qY_L <- -ky / sqrt(ny)
  qY_U <-  ky / sqrt(ny)
  
  qX_L <- -kx / sqrt(nx)
  qX_U <-  kx / sqrt(nx)
  
  qW_L <- -wx / sqrt(nx)
  qW_U <-  wx / sqrt(nx)
  
  # in-control probabilities under skew-normal data
  pY_in_0 <- p_zbar_between_sim(qY_L, qY_U, ny, alpha_y, B = B, seed = seed_y)
  
  pX_between_0 <-
    p_zbar_between_sim(qX_L, qW_L, nx, alpha_x, B = B, seed = seed_x) +
    p_zbar_between_sim(qW_U, qX_U, nx, alpha_x, B = B, seed = seed_x)
  
  # out-of-control probabilities under skew-normal data
  pY_in_1 <- p_zbar_between_sim(qY_L - delta_y, qY_U - delta_y,
                                ny, alpha_y, B = B, seed = seed_y)
  
  pX_warn_1 <- p_zbar_between_sim(qW_L - delta_x, qW_U - delta_x,
                                  nx, alpha_x, B = B, seed = seed_x)
  
  pX_between_1 <-
    p_zbar_between_sim(qX_L - delta_x, qW_L - delta_x,
                       nx, alpha_x, B = B, seed = seed_x) +
    p_zbar_between_sim(qW_U - delta_x, qX_U - delta_x,
                       nx, alpha_x, B = B, seed = seed_x)
  
  # transition matrix
  p11 <- pY_in_0 * phys
  p12 <- (1 - pY_in_0) * phxs
  p13 <- pY_in_0 * phy
  p14 <- (1 - pY_in_0) * phx
  
  p21 <- (1 - pX_between_0) * phys
  p22 <- pX_between_0 * phxs
  p23 <- (1 - pX_between_0) * phy
  p24 <- pX_between_0 * phx
  
  p33 <- pY_in_1
  p34 <- 1 - p33
  
  p43 <- pX_warn_1
  p44 <- pX_between_1
  
  Q <- matrix(c(
    p11, p12, p13, p14,
    p21, p22, p23, p24,
    0,   0,   p33, p34,
    0,   0,   p43, p44
  ), nrow = 4, byrow = TRUE)
  
  IQT <- solve(diag(4) - Q)
  mv  <- bT %*% IQT
  ATS <- as.numeric(bT %*% IQT %*% c(hy, hx, hy, hx))
  
  my0 <- mv[1, 1]
  mx0 <- mv[1, 2]
  my1 <- mv[1, 3]
  mx1 <- mv[1, 4]
  
  # false alarm under wrong normal chart but skew data
  pFA_x <- 1 - p_zbar_between_sim(qX_L, qX_U, nx, alpha_x, B = B, seed = seed_x)
  EFA <- mx0 * pFA_x
  
  ET <- ATS + b3x * nx + b3y * ny + b2 * EFA + b1
  
  EIv <- i1 / theta +
    i2 * (ATS - 1 / theta + b3x * nx + b3y * ny) -
    eta1 - eta2 * EFA -
    (eta3x + eta4x * nx) * (mx0 + mx1) -
    (eta3y + eta4y * ny) * (my0 + my1)
  
  EA <- EIv / ET
  
  list(
    EA = EA,
    EIv = EIv,
    ATS = ATS,
    ET = ET,
    EFA = EFA,
    Q = Q,
    probs = list(
      pY_in_0 = pY_in_0,
      pX_between_0 = pX_between_0,
      pY_in_1 = pY_in_1,
      pX_warn_1 = pX_warn_1,
      pX_between_1 = pX_between_1,
      pFA_x = pFA_x
    )
  )
}

maximize_exp_income_skewdata_normalchart <- function(beta1, theta, shift_size, b1, b2, b3x,
                                                     i1, i2, eta1, eta2, eta3x, eta4x,
                                                     alpha_x = 2, alpha_y = 2,
                                                     sigma_zeta = 1,
                                                     sigma_x0 = 1,
                                                     sigma_y0 = 1,
                                                     B = 10000,
                                                     seed_x = 123,
                                                     seed_y = 456) {
  
  objective <- function(par) {
    nx <- max(1, round(par[1]))
    ny <- max(1, round(par[2]))
    hx <- par[3]
    hy <- par[4]
    kx <- par[5]
    ky <- par[6]
    wx <- par[7]
    
    if (wx <= 0 || wx >= kx ||
        b3x * nx > hx ||
        (0.2 * b3x * ny) > hy ||
        hx < 0.01 || hx > 20 ||
        hy < 0.01 || hy > 20 ||
        kx < 0.01 || kx > 4 ||
        ky < 0.01 || ky > 4) {
      return(Inf)
    }
    
    val <- tryCatch(
      exp_income_skewdata_normalchart(
        nx, ny, hx, hy, kx, ky, wx,
        beta1, theta, shift_size,
        b1, b2, b3x,
        i1, i2, eta1, eta2, eta3x, eta4x,
        alpha_x = alpha_x, alpha_y = alpha_y,
        sigma_zeta = sigma_zeta,
        sigma_x0 = sigma_x0,
        sigma_y0 = sigma_y0,
        B = B,
        seed_x = seed_x,
        seed_y = seed_y
      ),
      error = function(e) NULL
    )
    
    if (is.null(val) || !is.finite(val$EA)) return(Inf)
    -as.numeric(val$EA)
  }
  
  lower <- c(1, 1, 0.01, 0.01, 0.01, 0.01, 0.01)
  upper <- c(100, 100, 20, 20, 4, 4, 4)
  
  set.seed(2025)
  result <- DEoptim(
    fn = objective,
    lower = lower,
    upper = upper,
    control = DEoptim.control(trace = TRUE, itermax = 300)
  )
  
  best <- result$optim$bestmem
  best[1:2] <- round(best[1:2])
  
  final <- exp_income_skewdata_normalchart(
    best[1], best[2], best[3], best[4], best[5], best[6], best[7],
    beta1, theta, shift_size,
    b1, b2, b3x,
    i1, i2, eta1, eta2, eta3x, eta4x,
    alpha_x = alpha_x, alpha_y = alpha_y,
    sigma_zeta = sigma_zeta,
    sigma_x0 = sigma_x0,
    sigma_y0 = sigma_y0,
    B = B,
    seed_x = seed_x,
    seed_y = seed_y
  )
  
  list(opt_par = best, EA = final$EA, details = final, raw = result)
}


# ------------------------------------------------------------
# helper: run both methods for one example row
# ------------------------------------------------------------
run_skew_vs_normal_comparison <- function(theta, shift_size, b1, b2, b3x,
                                          i1, i2, eta1, eta2, eta3x, eta4x,
                                          alpha_x = 2, alpha_y = 2,
                                          beta1 = 0.3,
                                          sigma_zeta = 1,
                                          sigma_x0 = 1,
                                          sigma_y0 = 1,
                                          B = 100000,
                                          seed_x = 123,
                                          seed_y = 456) {
  
  # Proposed skew-normal chart
  res_skew <- maximize_exp_income_skew(
    beta1 = beta1,
    theta = theta,
    shift_size = shift_size,
    b1 = b1,
    b2 = b2,
    b3x = b3x,
    i1 = i1,
    i2 = i2,
    eta1 = eta1,
    eta2 = eta2,
    eta3x = eta3x,
    eta4x = eta4x,
    alpha_x = alpha_x,
    alpha_y = alpha_y,
    sigma_zeta = sigma_zeta,
    sigma_x0 = sigma_x0,
    sigma_y0 = sigma_y0,
    B = B,
    seed_x = seed_x,
    seed_y = seed_y
  )
  
  
  
  # Costa normal chart on skewed data
  res_normal <- maximize_exp_income_skewdata_normalchart(
    beta1 = beta1,
    theta = theta,
    shift_size = shift_size,
    b1 = b1,
    b2 = b2,
    b3x = b3x,
    i1 = i1,
    i2 = i2,
    eta1 = eta1,
    eta2 = eta2,
    eta3x = eta3x,
    eta4x = eta4x,
    alpha_x = alpha_x,
    alpha_y = alpha_y,
    sigma_zeta = sigma_zeta,
    sigma_x0 = sigma_x0,
    sigma_y0 = sigma_y0,
    B = B,
    seed_x = seed_x,
    seed_y = seed_y
  )
  
  tibble(
    # proposed skew-normal chart
    nx_skew   = round(res_skew$opt_par[1]),
    ny_skew   = round(res_skew$opt_par[2]),
    hx_skew   = res_skew$opt_par[3],
    hy_skew   = res_skew$opt_par[4],
    kx_skew   = res_skew$opt_par[5],
    ky_skew   = res_skew$opt_par[6],
    wx_skew   = res_skew$opt_par[7],
    EA_skew   = res_skew$EA,
    ATS_skew  = res_skew$details$ATS,
    EFA_skew  = res_skew$details$EFA,
    
    # Costa normal chart under skewed data
    nx_norm   = round(res_normal$opt_par[1]),
    ny_norm   = round(res_normal$opt_par[2]),
    hx_norm   = res_normal$opt_par[3],
    hy_norm   = res_normal$opt_par[4],
    kx_norm   = res_normal$opt_par[5],
    ky_norm   = res_normal$opt_par[6],
    wx_norm   = res_normal$opt_par[7],
    EA_norm   = res_normal$EA,
    ATS_norm  = res_normal$details$ATS,
    EFA_norm  = res_normal$details$EFA,
    
    # differences
    EA_diff   = res_normal$EA - res_skew$EA,
    ATS_diff  = res_normal$details$ATS - res_skew$details$ATS
  )
}

