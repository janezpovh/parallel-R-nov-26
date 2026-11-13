library(future)
library(future.apply)

f <- function(i) {
  x <- rnorm(2e6)
  c(task = i, statistic = mean(exp(x)))
}

plan(sequential)
a <- future_lapply(1:8, f, future.seed = TRUE)

plan(multisession, workers = 4)
b <- future_lapply(1:8, f, future.seed = TRUE)

identical(a, b)
plan(sequential)

library(future)
library(furrr)

plan(multisession, workers = 4)

results <- future_map(
  1:20,
  ~ mean(rnorm(1e6, mean = .x)),
  .options = furrr_options(seed = 2026)
)

plan(sequential)
