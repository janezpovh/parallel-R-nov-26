needed.packages <- c("foreach", "doParallel","parallel","tictoc","pracma","microbenchmark","parallely","future","future.apply")
new.packages <- needed.packages[!(needed.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(future)
library(future.apply)

f <- function(i) {
  x <- rnorm(2e6)
  c(task = i, statistic = mean(exp(x)))
}

plan(sequential)
a <- future_lapply(1:8, f, future.seed = TRUE)

plan(multisession, workers = 2)
b <- future_lapply(1:8, f, future.seed = TRUE)

identical(a, b)
plan(sequential)

library(future)
library(furrr)

plan(multisession, workers = 2)

results <- future_map(
  1:20,
  ~ mean(rnorm(1e6, mean = .x)),
  .options = furrr_options(seed = 2026)
)

plan(sequential)
