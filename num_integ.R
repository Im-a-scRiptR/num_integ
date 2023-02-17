# Packages                      ----
cat("\014")
rm(list = ls())
# library(tidyverse)
pacman::p_load(dplyr,magrittr,tibble,stringr,slider,stats,pracma)

# Numeric Integration Functions ----

mp_int <-
  \(f, a, b, n) {
    dx  <-  ((b - a) / n)
    pts <- seq(a, b, dx)
    mid_pts <-
      slider::slide_mean(seq(a, b, dx), before = 0, after = 1)[-length(pts)]
    return(dx * (mid_pts %>% sapply(\(x) f(x)) %>% sum()))
  }

tp_int <- \(f, a, b, n) {
  dx  <-  ((b - a) / n)
  pts <- seq(a, b, dx)
  no_scale <-
    c(pts[1], pts[length(pts)]) %>% sapply(\(x) f(x)) %>% sum()
  yes_scale <-
    pts[2:(length(pts) - 1)] %>% sapply(\(x) 2 * f(x)) %>% sum()
  return((dx / 2) * (no_scale + yes_scale))
}

sp_int <- \(f, a, b, n) {
  dx <- (b - a) / n
  pts <- seq(a, b, dx)
  no_scale <-
    c(head(pts, 1), tail(pts, 1)) %>%
    sapply(\(x) f(x)) %>% sum()
  pts <- pts[-c(1, length(pts))]
  two_scale <-
    pts[1:length(pts) %>% sapply(\(x) x %% 2 == 0)] %>%
    sapply(\(x) f(x)) %>% {
      (. * 2)
    } %>% sum()
  four_scale <-
    pts[1:length(pts) %>% sapply(\(x) x %% 2 != 0)] %>%
    sapply(\(x) f(x)) %>% {
      (. * 4)
    } %>% sum()
  return((dx / 3) * (no_scale + two_scale + four_scale))
}

tp_err <- \(K, b, a, n) {
  (K * ((b - a) ^ 3)) / (12 * n ^ 2)
}

mp_err <- \(K, b, a, n) {
  (K * ((b - a) ^ 3)) / (24 * n ^ 2)
}

sp_err <- \(K, b, a, n) {
  (K * ((b - a) ^ 5)) / (180 * n ^ 4)
}

opt_int_n <- \(K, b, a, n, error_fun) {
  err_fun <- match.fun(error_fun)
  err_fun(K, b, a, n)
}

int_err_fun_opt_n <- \(K, b, a, max_n, max_error) {
  c("tp_err","mp_err","sp_err") %>% 
    lapply(\(error_fun) {
      out <- 
        tibble::tibble(min_n = 1:max_n) %>% 
        dplyr::mutate(error = opt_int_n(K, b, a, min_n, error_fun)) %>% 
        dplyr::mutate(error_fun = error_fun) 
      if(out$error_fun[1] == "sp_err") {
        out <- out %>% dplyr::slice(seq_len(max_n)[(seq_len(max_n) %% 2 == 0)])
      }
      out <- 
        out %>% 
        dplyr::filter(error < max_error) %>% dplyr::slice(which.max(error)) %>% 
        dplyr::mutate(K = K, b = b, a = a)
    }) %>% 
    dplyr::bind_rows()
}

int_err_find_K <- \(f, b, a, n, error_fun, var_name) {
  dx <- (b - a) / n
  f_tex <-
    body(f) %>%
    as.character() %>%
    .[[2]] %>%
    parse(text = .)
  f_d2 <-  \(x) {
    # Leave Empty
  }
  if(error_fun != "sp_err") {
    body(f_d2) <- 
      stats::D(
        stats::D(
          f_tex, var_name), 
        var_name)
    new_f <- f_d2
  } else {
    body(f_d2) <- 
      stats::D(
        stats::D(
          stats::D(
            stats::D(
              f_tex, var_name), 
            var_name), 
          var_name), 
        var_name)
    new_f <- f_d2
  }
  pts <- seq(a, b, dx)
  return(list(pts %>% sapply(\(x) abs(new_f(
    x
  ))) %>% max(), new_f))
}

# Testing                       ----

# Define Function to Integrate
f <- \(x) {
  11 * exp(x^2)
}

# Define Integration Parameters
a         <- 0
b         <- 1
n         <- 100
max_n     <- 10000
max_error <- 1e-4

# Numerically Integrate
tp_int(f, a, b, n) %>% print(digits = 10)
mp_int(f, a, b, n) %>% print(digits = 10)
sp_int(f, a, b, n) %>% print(digits = 10)

# Find K for error forumlas
K    <- int_err_find_K(f,b,a,n,"sp_err",'x')
f_d2 <- K[[2]]
K    <- K[[1]]

# Calculate errors
tp_err(K,b,a,n) %>% print(digits = 10)
mp_err(K,b,a,n) %>% print(digits = 10)
sp_err(K,b,a,n) %>% print(digits = 10)

# Find optimal n
int_err_fun_opt_n(K, b, a, max_n, max_error)

# Test against specialized software
pracma::integral(f,a,b,"Simpson")
